use std::{
    cmp::Reverse,
    collections::{BTreeMap, BTreeSet},
    fs::{read_to_string, write},
    path::Path,
};

use anyhow::{Context, Result};
use similar::{ChangeTag, TextDiff};
use syn::{
    Attribute, ExprCall, ExprClosure, File, ImplItemFn, Item, ItemConst, ItemEnum, ItemFn,
    ItemImpl, ItemMod, ItemStatic, ItemStruct, ItemTrait, ItemType, ItemUnion, ItemUse, Local,
    Macro, Pat, PatIdent, Path as SynPath, TypePath, UseTree, parse_file,
    spanned::Spanned,
    visit::{
        Visit, {self},
    },
    visit_mut::{
        VisitMut, {self},
    },
};
use visit_mut::{visit_local_mut, visit_pat_ident_mut};

/// Extract #[cfg(...)] attributes from an attribute list.
/// Returns a vector of cfg attribute strings (e.g., ["cfg(test)", "cfg(unix)"]).
fn extract_cfg_attrs(attrs: &[Attribute]) -> Vec<String> {
    attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("cfg") {
                let list = attr.meta.require_list().ok()?;
                Some(format!("cfg({})", list.tokens))
            } else {
                None
            }
        })
        .collect()
}

/// Rust primitive types that cannot be imported via `use` statements.
/// These include numeric types, bool, char, str, and never type.
const PRIMITIVE_TYPES: &[&str] = &[
    "bool", "char", "str", "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64",
    "u128", "usize", "f32", "f64",
];

/// Rust prelude types that are automatically in scope.
/// These types may be used unqualified in code, and importing a different type
/// with the same name would shadow them, causing subtle bugs.
/// For example, `use std::fmt::Result;` would shadow `std::result::Result`.
const PRELUDE_TYPES: &[&str] = &[
    // std::option
    "Option",
    // std::result
    "Result",
    // std::boxed
    "Box",
    // std::string
    "String",
    // std::vec
    "Vec",
];

// A single occurrence of a path in the source that needs rewriting.
#[derive(Clone, Debug)]
struct PathOccurrence {
    full_path_str: String,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
    /// The scope this occurrence is in ("" for file level, "tests" for mod tests, etc.)
    scope: String,
    /// The #[cfg(...)] attributes that apply to this occurrence.
    /// Multiple cfg attributes are combined (all must be satisfied).
    /// Empty means no cfg restriction.
    cfg_attrs: Vec<String>,
}

/// Information about a scope (file level or module) for use statement insertion.
#[derive(Debug)]
struct ScopeInfo {
    /// Byte position where use statements should be inserted
    insert_pos: usize,
    /// Existing imported identifiers in this scope
    imported_idents: BTreeSet<String>,
    /// Indentation prefix for use statements in this scope (e.g., "    " for 4 spaces)
    indent: String,
}

// Collect all candidate fully-qualified paths from function calls.
struct PathCollector<'a> {
    paths: BTreeSet<String>,
    occurrences: Vec<PathOccurrence>,
    ignore_roots: &'a BTreeSet<String>,
    /// Current scope path segments (e.g., ["tests"] when inside `mod tests { }`)
    scope_stack: Vec<String>,
    /// Per-scope information for use statement insertion
    scope_infos: BTreeMap<String, ScopeInfo>,
    /// Line offsets for byte position calculation
    line_offsets: &'a LineOffsets,
    /// Mapping from imported names to their full paths (file-level imports only).
    /// e.g., `use tokio::task;` creates mapping: "task" → "tokio::task"
    /// This allows expanding `task::spawn` to `tokio::task::spawn`.
    import_mappings: BTreeMap<String, String>,
    /// Names that are imported locally (inside functions/blocks) from internal paths
    /// (`crate::`, `self::`, `super::`). Paths starting with these names should be skipped
    /// since they're already resolved by a local import and expanding them would just
    /// create redundant file-level imports.
    locally_imported_internal_names: BTreeSet<String>,
    /// Depth of nested blocks (functions, closures, blocks). When > 0, we're inside a block scope.
    block_depth: usize,
    /// Stack of #[cfg(...)] attributes currently in scope.
    /// Each entry is the string representation of a cfg attribute (e.g., "cfg(test)").
    cfg_stack: Vec<String>,
}

impl<'a> PathCollector<'a> {
    fn new(ignore_roots: &'a BTreeSet<String>, line_offsets: &'a LineOffsets) -> Self {
        Self {
            paths: BTreeSet::new(),
            occurrences: Vec::new(),
            ignore_roots,
            scope_stack: Vec::new(),
            scope_infos: BTreeMap::new(),
            line_offsets,
            import_mappings: BTreeMap::new(),
            locally_imported_internal_names: BTreeSet::new(),
            block_depth: 0,
            cfg_stack: Vec::new(),
        }
    }

    /// Get current cfg attributes as a sorted, deduplicated vector.
    fn current_cfg_attrs(&self) -> Vec<String> {
        let mut attrs: Vec<String> = self.cfg_stack.clone();
        attrs.sort();
        attrs.dedup();
        attrs
    }

    /// Check if a name is locally imported from an internal path (crate/self/super).
    fn is_locally_imported_internal(&self, name: &str) -> bool {
        self.locally_imported_internal_names.contains(name)
    }

    /// Check if we're currently inside a block scope (function body, closure, etc.)
    fn is_in_block_scope(&self) -> bool {
        self.block_depth > 0
    }

    /// Get the current scope as a string (e.g., "" for file level, "tests" for mod tests)
    fn current_scope(&self) -> String {
        self.scope_stack.join("::")
    }

    /// Try to expand a path using import mappings.
    /// e.g., if `task` maps to `tokio::task`, then `task::spawn` expands to `tokio::task::spawn`.
    /// Returns the expanded path if the first segment is imported, otherwise None.
    ///
    /// However, if the first segment of the path matches the first segment of the expanded path
    /// (e.g., `anyhow` in both `anyhow::Error` and `anyhow::anyhow`), we don't expand because
    /// the user is referencing the crate directly, not the imported alias.
    fn expand_path(&self, first_segment: &str, remaining_segments: &[String]) -> Option<String> {
        self.import_mappings.get(first_segment).and_then(|base_path| {
            // Check if the first segment of the base_path equals the lookup key.
            // If so, we have a case like `use anyhow::anyhow;` where "anyhow" maps to "anyhow::anyhow".
            // When we see `anyhow::Error`, we should NOT expand it to `anyhow::anyhow::Error`.
            let base_first = base_path.split("::").next().unwrap_or("");
            if base_first == first_segment && !remaining_segments.is_empty() {
                // The path `anyhow::Error` refers to the crate, not the imported `anyhow::anyhow`
                return None;
            }

            Some(if remaining_segments.is_empty() {
                base_path.clone()
            } else {
                format!("{base_path}::{}", remaining_segments.join("::"))
            })
        })
    }
}

impl Visit<'_> for PathCollector<'_> {
    // Collect import mappings from `use` statements.
    // All imports (file-level and block-level) are stored in `import_mappings` for path expansion.
    // Block-level imports of internal paths (crate/self/super) are additionally tracked to skip
    // them.
    fn visit_item_use(&mut self, node: &ItemUse) {
        // Always collect mappings for path expansion
        collect_use_mappings(&node.tree, &Vec::new(), &mut self.import_mappings);

        if self.is_in_block_scope() && is_internal_use_tree(&node.tree) {
            // This is a local import of an internal path - track to skip later
            collect_use_idents(&node.tree, &mut self.locally_imported_internal_names);
        }
        visit::visit_item_use(self, node);
    }

    // Track when entering function bodies and their cfg attributes
    fn visit_item_fn(&mut self, node: &syn::ItemFn) {
        let cfg_attrs = extract_cfg_attrs(&node.attrs);
        let cfg_count = cfg_attrs.len();
        self.cfg_stack.extend(cfg_attrs);
        self.block_depth += 1;
        visit::visit_item_fn(self, node);
        self.block_depth -= 1;
        self.cfg_stack.truncate(self.cfg_stack.len() - cfg_count);
    }

    // Track when entering impl method bodies and their cfg attributes
    fn visit_impl_item_fn(&mut self, node: &ImplItemFn) {
        let cfg_attrs = extract_cfg_attrs(&node.attrs);
        let cfg_count = cfg_attrs.len();
        self.cfg_stack.extend(cfg_attrs);
        self.block_depth += 1;
        visit::visit_impl_item_fn(self, node);
        self.block_depth -= 1;
        self.cfg_stack.truncate(self.cfg_stack.len() - cfg_count);
    }

    // Track when entering closures
    fn visit_expr_closure(&mut self, node: &ExprClosure) {
        self.block_depth += 1;
        visit::visit_expr_closure(self, node);
        self.block_depth -= 1;
    }

    // Track cfg attributes on impl blocks
    fn visit_item_impl(&mut self, node: &ItemImpl) {
        let cfg_attrs = extract_cfg_attrs(&node.attrs);
        let cfg_count = cfg_attrs.len();
        self.cfg_stack.extend(cfg_attrs);
        visit::visit_item_impl(self, node);
        self.cfg_stack.truncate(self.cfg_stack.len() - cfg_count);
    }

    fn visit_expr_call(&mut self, node: &ExprCall) {
        if let syn::Expr::Path(expr_path) = &*node.func
            && expr_path.qself.is_none()
        {
            let path = &expr_path.path;
            let segments = &path.segments;
            if segments.len() >= 2 {
                let first_ident = &segments[0].ident;
                let first_name = first_ident.to_string();

                // Skip paths that start with a locally imported internal name (crate/self/super).
                // They're already resolved by a local `use` statement and expanding them
                // would just create redundant file-level imports.
                if self.is_locally_imported_internal(&first_name) {
                    visit::visit_expr_call(self, node);
                    return;
                }

                // Try to expand the path if the first segment is a file-level import.
                // e.g., if `use tokio::task;` then `task::spawn` expands to `tokio::task::spawn`
                let remaining: Vec<String> =
                    segments.iter().skip(1).map(|s| s.ident.to_string()).collect();
                let (full_str, effective_first_name) =
                    if let Some(expanded) = self.expand_path(&first_name, &remaining) {
                        // Path was expanded - use the expanded version
                        let expanded_first =
                            expanded.split("::").next().unwrap_or(&first_name).to_string();
                        (expanded, expanded_first)
                    } else {
                        // No expansion needed - use the original path
                        (path_to_string(path), first_name.clone())
                    };

                let first_char = effective_first_name.chars().next().unwrap_or('a');

                // Check if second-to-last segment is uppercase (type associated function)
                // e.g., pdf2svg::Cli::try_new() - Cli is the type
                // Use the expanded path for this check
                let expanded_segments: Vec<&str> = full_str.split("::").collect();
                let second_to_last = if expanded_segments.len() >= 2 {
                    expanded_segments[expanded_segments.len() - 2]
                } else {
                    ""
                };
                let second_to_last_char = second_to_last.chars().next().unwrap_or('a');

                // Skip primitive types - they cannot be imported via `use`
                let is_primitive_method = PRIMITIVE_TYPES.contains(&second_to_last);

                if effective_first_name != "Self"
                    && !first_char.is_uppercase()
                    && !second_to_last_char.is_uppercase()
                    && !is_primitive_method
                    && !self.ignore_roots.contains(&effective_first_name)
                {
                    // Get span info for the entire path expression
                    let span = expr_path.span();
                    let start = span.start();
                    let end = span.end();

                    self.paths.insert(full_str.clone());

                    self.occurrences.push(PathOccurrence {
                        full_path_str: full_str,
                        start_line: start.line,
                        start_col: start.column,
                        end_line: end.line,
                        end_col: end.column,
                        scope: self.current_scope(),
                        cfg_attrs: self.current_cfg_attrs(),
                    });
                }
            }
        }
        visit::visit_expr_call(self, node);
    }

    fn visit_item_mod(&mut self, node: &ItemMod) {
        // Track cfg attributes on the module
        let cfg_attrs = extract_cfg_attrs(&node.attrs);
        let cfg_count = cfg_attrs.len();
        self.cfg_stack.extend(cfg_attrs);

        // Only process inline modules (with content)
        if let Some((brace, items)) = &node.content {
            let mod_name = node.ident.to_string();
            self.scope_stack.push(mod_name);
            let current_scope = self.current_scope();

            // Find last use statement in this module and collect imported idents
            let mut last_use_line: Option<usize> = None;
            let mut imported_idents = BTreeSet::new();

            for item in items {
                if let Item::Use(item_use) = item {
                    let span = item_use.span();
                    let end_line = span.end().line;
                    last_use_line = Some(end_line);
                    collect_use_idents(&item_use.tree, &mut imported_idents);
                }
            }

            // Determine indentation from the first item in the module, or use default
            let indent = if let Some(first_item) = items.first() {
                let col = first_item.span().start().column;
                " ".repeat(col)
            } else {
                // No items, calculate based on nesting level (4 spaces per level)
                " ".repeat(4 * self.scope_stack.len())
            };

            // Calculate insertion position
            let insert_pos = if let Some(line) = last_use_line {
                self.line_offsets.line_end_byte(line)
            } else {
                // Insert at the end of the line containing the opening brace
                // (so the new use statement starts on the next line with proper indentation)
                let brace_span = brace.span.open();
                self.line_offsets.line_end_byte(brace_span.end().line)
            };

            self.scope_infos
                .insert(current_scope, ScopeInfo { insert_pos, imported_idents, indent });

            // Visit children
            visit::visit_item_mod(self, node);

            self.scope_stack.pop();
        } else {
            // External module (mod foo;) - just visit normally
            visit::visit_item_mod(self, node);
        }

        self.cfg_stack.truncate(self.cfg_stack.len() - cfg_count);
    }

    fn visit_macro(&mut self, node: &Macro) {
        let path = &node.path;
        let segments = &path.segments;

        // Only process qualified macro paths (e.g., anyhow::bail!, tokio::select!)
        if segments.len() >= 2 {
            let first_ident = &segments[0].ident;
            let first_name = first_ident.to_string();

            // Skip paths that start with a locally imported internal name (crate/self/super).
            // They're already resolved by a local `use` statement and expanding them
            // would just create redundant file-level imports.
            if self.is_locally_imported_internal(&first_name) {
                visit::visit_macro(self, node);
                return;
            }

            // Try to expand the path if the first segment is a file-level import.
            let remaining: Vec<String> =
                segments.iter().skip(1).map(|s| s.ident.to_string()).collect();
            let (full_str, effective_first_name) = if let Some(expanded) =
                self.expand_path(&first_name, &remaining)
            {
                let expanded_first = expanded.split("::").next().unwrap_or(&first_name).to_string();
                (expanded, expanded_first)
            } else {
                (path_to_string(path), first_name.clone())
            };

            let first_char = effective_first_name.chars().next().unwrap_or('a');

            if effective_first_name != "Self"
                && !first_char.is_uppercase()
                && !self.ignore_roots.contains(&effective_first_name)
            {
                // Get span info for just the path (not including ! and args)
                let span = path.segments.span();
                let start = span.start();
                let end = span.end();

                self.paths.insert(full_str.clone());

                self.occurrences.push(PathOccurrence {
                    full_path_str: full_str,
                    start_line: start.line,
                    start_col: start.column,
                    end_line: end.line,
                    end_col: end.column,
                    scope: self.current_scope(),
                    cfg_attrs: self.current_cfg_attrs(),
                });
            }
        }
        visit::visit_macro(self, node);
    }

    fn visit_type_path(&mut self, node: &TypePath) {
        // Skip qualified self types like <Foo as Bar>::Baz
        if node.qself.is_some() {
            visit::visit_type_path(self, node);
            return;
        }

        let path = &node.path;
        let segments = &path.segments;

        // Only process qualified type paths (e.g., anyhow::Result, std::collections::HashMap)
        if segments.len() >= 2 {
            let first_ident = &segments[0].ident;
            let first_name = first_ident.to_string();

            // Skip paths that start with a locally imported internal name (crate/self/super).
            if self.is_locally_imported_internal(&first_name) {
                visit::visit_type_path(self, node);
                return;
            }

            // Try to expand the path if the first segment is a file-level import.
            let remaining: Vec<String> =
                segments.iter().skip(1).map(|s| s.ident.to_string()).collect();
            let (full_str, effective_first_name) = if let Some(expanded) =
                self.expand_path(&first_name, &remaining)
            {
                let expanded_first = expanded.split("::").next().unwrap_or(&first_name).to_string();
                (expanded, expanded_first)
            } else {
                (path_to_string(path), first_name.clone())
            };

            let first_char = effective_first_name.chars().next().unwrap_or('a');

            // Get the last segment (the type name)
            let last_segment = segments.last().map(|s| s.ident.to_string()).unwrap_or_default();

            // Skip primitive types and Self
            if effective_first_name != "Self"
                && !first_char.is_uppercase()
                && !PRIMITIVE_TYPES.contains(&last_segment.as_str())
                && !self.ignore_roots.contains(&effective_first_name)
            {
                // Get span info for the path, excluding generic arguments.
                // We use the start of the first segment and the end of the last segment's ident
                // to avoid capturing generic parameters like <T>.
                let start = segments.first().unwrap().ident.span().start();
                let end = segments.last().unwrap().ident.span().end();

                self.paths.insert(full_str.clone());

                self.occurrences.push(PathOccurrence {
                    full_path_str: full_str,
                    start_line: start.line,
                    start_col: start.column,
                    end_line: end.line,
                    end_col: end.column,
                    scope: self.current_scope(),
                    cfg_attrs: self.current_cfg_attrs(),
                });
            }
        }
        visit::visit_type_path(self, node);
    }
}

/// Represents a path's import strategy at a given level.
/// `import_len` is how many segments to import (from the start).
/// For path `a::b::c::func` with 4 segments:
/// - `import_len=4`: `use a::b::c::func;` → `func()`
/// - `import_len=3`: `use a::b::c;` → `c::func()`
/// - `import_len=2`: `use a::b;` → `b::c::func()`
/// - `import_len=1`: `use a;` → `a::b::c::func()` (same as original, skip)
#[derive(Clone)]
struct ImportStrategy {
    full_path: String,
    segments: Vec<String>,
    import_len: usize,
}

impl ImportStrategy {
    fn new(full_path: &str) -> Self {
        let segments: Vec<String> = full_path.split("::").map(String::from).collect();
        let import_len = segments.len();
        Self {
            full_path: full_path.to_string(),
            segments,
            import_len,
        }
    }

    /// The identifier that will be imported (brought into scope)
    fn import_ident(&self) -> &str {
        &self.segments[self.import_len - 1]
    }

    /// Try to go up one level (import less, use more prefix in call)
    fn go_up(&mut self) -> bool {
        if self.import_len > 1 {
            self.import_len -= 1;
            true
        } else {
            false
        }
    }

    /// Check if this strategy results in same call as original (no benefit)
    fn is_same_as_original(&self) -> bool {
        self.import_len == 1
    }

    /// Generate use statement
    fn use_statement(&self) -> String {
        format!("use {};", self.segments[..self.import_len].join("::"))
    }

    /// Generate call replacement text
    fn replacement(&self) -> String {
        if self.import_len == self.segments.len() {
            // Import the function itself
            self.segments.last().unwrap().clone()
        } else {
            // Import a parent, use prefix::...::func
            format!(
                "{}::{}",
                self.segments[self.import_len - 1],
                self.segments[self.import_len..].join("::")
            )
        }
    }
}

/// Multi-pass conflict resolution.
/// Groups paths by their import identifier and bumps conflicting groups up one level.
fn resolve_all_imports(
    paths: &[String],
    existing_idents: &BTreeSet<String>,
) -> BTreeMap<String, ImportStrategy> {
    let mut strategies: Vec<ImportStrategy> =
        paths.iter().map(|p| ImportStrategy::new(p)).collect();

    // Phase 1: Resolve conflicts between new imports
    // Keep bumping until no more internal conflicts
    loop {
        // Group by import_ident (only for paths that aren't at same-as-original level)
        let mut groups: BTreeMap<String, Vec<usize>> = BTreeMap::new();
        for (idx, strategy) in strategies.iter().enumerate() {
            if !strategy.is_same_as_original() {
                groups
                    .entry(strategy.import_ident().to_string())
                    .or_default()
                    .push(idx);
            }
        }

        // Find groups with internal conflicts (size > 1)
        let mut has_conflict = false;
        for indices in groups.values() {
            if indices.len() > 1 {
                // Bump all members of this group up one level
                for &idx in indices {
                    if strategies[idx].go_up() {
                        has_conflict = true;
                    }
                }
            }
        }

        if !has_conflict {
            break;
        }
    }

    // Phase 2: Handle conflicts with existing idents by going up levels
    loop {
        let mut has_conflict = false;
        for strategy in &mut strategies {
            if !strategy.is_same_as_original()
                && existing_idents.contains(strategy.import_ident())
                && strategy.go_up()
            {
                has_conflict = true;
            }
        }
        if !has_conflict {
            break;
        }
    }

    // Phase 3: Build result, excluding invalid strategies
    let mut used_idents: BTreeSet<String> = BTreeSet::new();
    let mut result: BTreeMap<String, ImportStrategy> = BTreeMap::new();

    for strategy in strategies {
        // Skip if it would be same as original (no benefit)
        if strategy.is_same_as_original() {
            continue;
        }

        let import_ident = strategy.import_ident().to_string();

        // Skip if this ident is already used by another path
        if used_idents.contains(&import_ident) {
            continue;
        }

        used_idents.insert(import_ident);
        result.insert(strategy.full_path.clone(), strategy);
    }

    result
}

/// Represents an edit to apply to the source.
#[derive(Debug)]
enum Edit {
    /// Insert text at a position
    Insert { pos: usize, text: String },
    /// Replace a range with new text
    Replace { start: usize, end: usize, text: String },
}

impl Edit {
    /// Get the position used for sorting (we sort descending to apply from end to start)
    fn sort_position(&self) -> usize {
        match self {
            Edit::Insert { pos, .. } => *pos,
            Edit::Replace { start, .. } => *start,
        }
    }
}

// Process a single file.
// Returns Some(diff_string) if the file would be / was changed, None otherwise.
// In write mode, the diff string is empty but Some indicates changes occurred.
pub fn process_file(path: &Path, ignore_roots: &[String], dry_run: bool) -> Result<Option<String>> {
    let src = read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    let ast: File =
        parse_file(&src).with_context(|| format!("failed to parse {}", path.display()))?;

    let line_offsets = LineOffsets::new(&src);
    let ignore_set: BTreeSet<String> = ignore_roots.iter().cloned().collect();
    let mut collector = PathCollector::new(&ignore_set, &line_offsets);

    // Initialize file-level scope info (empty scope path "")
    let file_level_imported = collect_imported_idents(&ast);
    let file_level_insert_pos = find_use_insert_position(&ast, &line_offsets);
    collector.scope_infos.insert(
        String::new(),
        ScopeInfo {
            insert_pos: file_level_insert_pos,
            imported_idents: file_level_imported.clone(),
            indent: String::new(), // No indentation at file level
        },
    );

    collector.visit_file(&ast);

    if collector.paths.is_empty() {
        return Ok(None);
    }

    let local_idents = collect_local_idents(&ast);
    let used_prelude_types = collect_unqualified_prelude_usages(&ast);

    // Group occurrences by scope
    let mut occ_by_scope: BTreeMap<&str, Vec<&PathOccurrence>> = BTreeMap::new();
    for occ in &collector.occurrences {
        occ_by_scope.entry(&occ.scope).or_default().push(occ);
    }

    let mut edits: Vec<Edit> = Vec::new();

    // Process each scope
    for (scope, occurrences) in &occ_by_scope {
        // Get scope info (if not found, use file-level - shouldn't happen but be safe)
        let scope_info = collector
            .scope_infos
            .get(*scope)
            .unwrap_or_else(|| collector.scope_infos.get("").unwrap());

        // Combine file-level imports with scope-specific imports for conflict detection
        let mut existing_idents: BTreeSet<String> = file_level_imported.clone();
        existing_idents.extend(scope_info.imported_idents.clone());
        existing_idents.extend(local_idents.clone());
        // Add prelude types that are actually used unqualified to prevent shadowing
        // (e.g., if `Result<T, E>` is used, we won't import `std::fmt::Result`)
        existing_idents.extend(used_prelude_types.clone());

        // Collect unique paths in this scope
        let scope_paths: BTreeSet<&str> =
            occurrences.iter().map(|o| o.full_path_str.as_str()).collect();
        let scope_paths_vec: Vec<String> = scope_paths.iter().map(|&s| s.to_owned()).collect();

        // Resolve imports for this scope
        let strategies = resolve_all_imports(&scope_paths_vec, &existing_idents);

        // Build use statements grouped by cfg attributes
        // Key: sorted cfg attrs joined (empty string for no cfg)
        // Value: list of use statements for that cfg group
        let mut use_by_cfg: BTreeMap<Vec<String>, BTreeSet<String>> = BTreeMap::new();

        for occ in occurrences {
            let Some(strategy) = strategies.get(&occ.full_path_str) else {
                continue;
            };

            // Add use statement to appropriate cfg group
            let use_stmt = strategy.use_statement();
            use_by_cfg.entry(occ.cfg_attrs.clone()).or_default().insert(use_stmt);

            // Create replacement for this occurrence
            let start_byte = line_offsets.line_col_to_byte(occ.start_line, occ.start_col);
            let end_byte = line_offsets.line_col_to_byte(occ.end_line, occ.end_col);

            if let (Some(start), Some(end)) = (start_byte, end_byte) {
                edits.push(Edit::Replace { start, end, text: strategy.replacement() });
            }
        }

        // Create use block insertion for this scope, grouped by cfg
        if !use_by_cfg.is_empty() {
            let indent = &scope_info.indent;
            let mut use_blocks: Vec<String> = Vec::new();

            for (cfg_attrs, use_stmts) in &use_by_cfg {
                let sorted_stmts: Vec<&String> = use_stmts.iter().collect();

                if cfg_attrs.is_empty() {
                    // No cfg - just add use statements directly
                    for stmt in sorted_stmts {
                        use_blocks.push(format!("{indent}{stmt}"));
                    }
                } else {
                    // Has cfg attrs - wrap each use statement with #[cfg(...)]
                    for cfg in cfg_attrs {
                        for stmt in &sorted_stmts {
                            use_blocks.push(format!("{indent}#[{cfg}]\n{indent}{stmt}"));
                        }
                    }
                }
            }

            if !use_blocks.is_empty() {
                let use_block = "\n".to_string() + &use_blocks.join("\n") + "\n";
                edits.push(Edit::Insert { pos: scope_info.insert_pos, text: use_block });
            }
        }
    }

    if edits.is_empty() {
        return Ok(None);
    }

    // Sort edits by position descending (apply from end to start)
    edits.sort_by_key(|e| Reverse(e.sort_position()));

    // Apply edits
    let mut new_src = src.clone();
    for edit in edits {
        match edit {
            Edit::Insert { pos, text } => {
                new_src.insert_str(pos, &text);
            }
            Edit::Replace { start, end, text } => {
                new_src.replace_range(start..end, &text);
            }
        }
    }

    if new_src == src {
        return Ok(None);
    }

    if dry_run {
        return Ok(Some(format_diff(path, &src, &new_src)));
    }

    write(path, &new_src).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(Some(String::new()))
}

fn path_to_string(path: &SynPath) -> String {
    path.segments
        .iter()
        .map(|s| s.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

// For each line, store (line_start_byte, Vec of char_index -> byte_offset_in_line).
// This handles UTF-8 correctly since proc_macro2::Span uses character columns, not byte columns.
struct LineOffsets {
    lines: Vec<(usize, Vec<usize>)>, // (line_start_byte, char_to_byte_offset)
}

impl LineOffsets {
    fn new(src: &str) -> Self {
        let mut lines: Vec<(usize, Vec<usize>)> = Vec::new();
        let mut current_char_offsets: Vec<usize> = Vec::new();
        let mut line_start_byte = 0usize;

        for (byte_idx, ch) in src.char_indices() {
            if ch == '\n' {
                lines.push((line_start_byte, current_char_offsets));
                current_char_offsets = Vec::new();
                line_start_byte = byte_idx + 1;
            } else {
                // Store byte offset relative to line start for this character
                current_char_offsets.push(byte_idx - line_start_byte);
            }
        }
        // Push the last line
        lines.push((line_start_byte, current_char_offsets));

        Self { lines }
    }

    // Convert (line, col) to byte offset. Line is 1-indexed, col is 0-indexed character column.
    fn line_col_to_byte(&self, line: usize, col: usize) -> Option<usize> {
        if line == 0 || line > self.lines.len() {
            return None;
        }
        let (line_start, char_offsets) = &self.lines[line - 1];
        if col >= char_offsets.len() {
            // Column is at or past end of line - return end of line
            // This can happen for end positions
            return Some(line_start + char_offsets.last().map_or(0, |&o| o + 1));
        }
        Some(line_start + char_offsets[col])
    }

    // Get the byte offset at the end of a line (position of newline or end of file).
    // Line is 1-indexed.
    fn line_end_byte(&self, line: usize) -> usize {
        if line == 0 || line > self.lines.len() {
            return 0;
        }
        if line < self.lines.len() {
            // The newline is at (start of next line - 1)
            self.lines[line].0 - 1
        } else {
            // Last line - no newline, use end of file
            // We don't have total length here, so fall back to line_col_to_byte approach
            // For last line, there's no trailing newline to worry about
            let (line_start, char_offsets) = &self.lines[line - 1];
            line_start + char_offsets.last().map_or(0, |&o| o + 1)
        }
    }
}

// Find the byte position where use statements should be inserted.
fn find_use_insert_position(ast: &File, line_offsets: &LineOffsets) -> usize {
    let mut last_use_line: Option<usize> = None;

    for item in &ast.items {
        if let Item::Use(item_use) = item {
            let span = item_use.span();
            let end_line = span.end().line;
            last_use_line = Some(end_line);
        }
    }

    // Return the byte position at the end of the line containing the last use statement.
    // This ensures we don't insert in the middle of trailing comments.
    if let Some(line) = last_use_line { line_offsets.line_end_byte(line) } else { 0 }
}

// Collect the identifiers that are already imported into the file via `use`.
fn collect_imported_idents(ast: &File) -> BTreeSet<String> {
    let mut idents = BTreeSet::new();
    for item in &ast.items {
        if let Item::Use(item_use) = item {
            collect_use_idents(&item_use.tree, &mut idents);
        }
    }
    idents
}

fn collect_use_idents(tree: &UseTree, out: &mut BTreeSet<String>) {
    match tree {
        UseTree::Name(n) => {
            out.insert(n.ident.to_string());
        }
        UseTree::Rename(r) => {
            out.insert(r.rename.to_string());
        }
        UseTree::Path(p) => {
            collect_use_idents(&p.tree, out);
        }
        UseTree::Group(g) => {
            for t in &g.items {
                collect_use_idents(t, out);
            }
        }
        UseTree::Glob(_g) => {}
    }
}

/// Check if a use tree starts with an internal path (crate, self, super).
fn is_internal_use_tree(tree: &UseTree) -> bool {
    match tree {
        UseTree::Path(p) => {
            let first = p.ident.to_string();
            first == "crate" || first == "self" || first == "super"
        }
        UseTree::Group(g) => {
            // A group at the root level (without a path prefix) is unusual,
            // but check all items just in case
            g.items.iter().all(is_internal_use_tree)
        }
        // Name/Rename/Glob at root level are external imports
        _ => false,
    }
}

/// Collect import mappings from a use tree.
/// Maps the imported name to its full path.
/// e.g., `use tokio::task;` creates mapping: `task` → `tokio::task`
/// e.g., `use tokio::task as t;` creates mapping: `t` → `tokio::task`
fn collect_use_mappings(tree: &UseTree, prefix: &[String], out: &mut BTreeMap<String, String>) {
    match tree {
        UseTree::Name(n) => {
            let name = n.ident.to_string();
            let mut full_path_parts: Vec<String> = prefix.to_vec();
            full_path_parts.push(name.clone());
            let full_path = full_path_parts.join("::");
            out.insert(name, full_path);
        }
        UseTree::Rename(r) => {
            // `use foo::bar as baz;` - baz maps to foo::bar
            let alias = r.rename.to_string();
            let original = r.ident.to_string();
            let mut full_path_parts: Vec<String> = prefix.to_vec();
            full_path_parts.push(original);
            let full_path = full_path_parts.join("::");
            out.insert(alias, full_path);
        }
        UseTree::Path(p) => {
            let segment = p.ident.to_string();
            let mut new_prefix: Vec<String> = prefix.to_vec();
            new_prefix.push(segment);
            collect_use_mappings(&p.tree, &new_prefix, out);
        }
        UseTree::Group(g) => {
            for t in &g.items {
                collect_use_mappings(t, prefix, out);
            }
        }
        UseTree::Glob(_g) => {
            // Glob imports don't create specific mappings we can track
        }
    }
}

// Collect local identifiers in the file to detect potential conflicts.
fn collect_local_idents(ast: &File) -> BTreeSet<String> {
    let mut set = BTreeSet::new();
    for item in &ast.items {
        match item {
            Item::Fn(ItemFn { sig, .. }) => {
                set.insert(sig.ident.to_string());
                for input in &sig.inputs {
                    if let syn::FnArg::Typed(pat_ty) = input {
                        collect_pattern_idents(&pat_ty.pat, &mut set);
                    }
                }
            }
            Item::Struct(ItemStruct { ident, .. })
            | Item::Enum(ItemEnum { ident, .. })
            | Item::Union(ItemUnion { ident, .. })
            | Item::Trait(ItemTrait { ident, .. })
            | Item::Type(ItemType { ident, .. })
            | Item::Mod(ItemMod { ident, .. }) => {
                set.insert(ident.to_string());
            }
            Item::Static(ItemStatic { ident, .. }) | Item::Const(ItemConst { ident, .. }) => {
                set.insert(ident.to_string());
            }
            Item::Impl(ItemImpl { items, .. }) => {
                for impl_item in items {
                    if let syn::ImplItem::Fn(f) = impl_item {
                        set.insert(f.sig.ident.to_string());
                    }
                }
            }
            _ => {}
        }
    }
    struct LocalBindingCollector<'a> {
        set: &'a mut BTreeSet<String>,
    }
    impl VisitMut for LocalBindingCollector<'_> {
        fn visit_local_mut(&mut self, node: &mut Local) {
            collect_pattern_idents(&node.pat, self.set);
            visit_local_mut(self, node);
        }
        fn visit_pat_ident_mut(&mut self, node: &mut PatIdent) {
            self.set.insert(node.ident.to_string());
            visit_pat_ident_mut(self, node);
        }
    }
    let mut ast_clone = ast.clone();
    let mut collector = LocalBindingCollector { set: &mut set };
    collector.visit_file_mut(&mut ast_clone);
    set
}

fn collect_pattern_idents(pat: &Pat, out: &mut BTreeSet<String>) {
    match pat {
        Pat::Ident(p) => {
            out.insert(p.ident.to_string());
        }
        Pat::Tuple(tuple) => {
            for p in &tuple.elems {
                collect_pattern_idents(p, out);
            }
        }
        Pat::Struct(s) => {
            for field in &s.fields {
                collect_pattern_idents(&field.pat, out);
            }
        }
        Pat::TupleStruct(ts) => {
            for p in &ts.elems {
                collect_pattern_idents(p, out);
            }
        }
        Pat::Slice(slice) => {
            for p in &slice.elems {
                collect_pattern_idents(p, out);
            }
        }
        Pat::Reference(r) => {
            collect_pattern_idents(&r.pat, out);
        }
        Pat::Or(or) => {
            for p in &or.cases {
                collect_pattern_idents(p, out);
            }
        }
        _ => {}
    }
}

/// Collect unqualified usages of prelude types in the AST.
/// Returns only prelude type names that are actually used unqualified in the file.
/// This prevents shadowing - e.g., if `Result<T, E>` is used, we won't import `std::fmt::Result`.
fn collect_unqualified_prelude_usages(ast: &File) -> BTreeSet<String> {
    struct PreludeUsageCollector {
        used_prelude_types: BTreeSet<String>,
    }

    impl<'ast> Visit<'ast> for PreludeUsageCollector {
        fn visit_type_path(&mut self, node: &'ast TypePath) {
            // Only check paths without a qself (not <T as Trait>::Type)
            if node.qself.is_none() {
                let segments: Vec<_> = node.path.segments.iter().collect();
                // Check for single-segment paths (unqualified usage)
                if segments.len() == 1 {
                    let ident = segments[0].ident.to_string();
                    if PRELUDE_TYPES.contains(&ident.as_str()) {
                        self.used_prelude_types.insert(ident);
                    }
                }
            }
            visit::visit_type_path(self, node);
        }
    }

    let mut collector = PreludeUsageCollector {
        used_prelude_types: BTreeSet::new(),
    };
    collector.visit_file(ast);
    collector.used_prelude_types
}

fn format_diff(path: &Path, old: &str, new: &str) -> String {
    use std::fmt::Write;
    let diff = TextDiff::from_lines(old, new);
    let mut output = String::new();
    writeln!(output, "--- {}", path.display()).unwrap();
    writeln!(output, "+++ {}", path.display()).unwrap();
    for hunk in diff.unified_diff().context_radius(3).iter_hunks() {
        writeln!(output, "{}", hunk.header()).unwrap();
        for change in hunk.iter_changes() {
            let sign = match change.tag() {
                ChangeTag::Delete => "-",
                ChangeTag::Insert => "+",
                ChangeTag::Equal => " ",
            };
            write!(output, "{sign}{change}").unwrap();
            if change.missing_newline() {
                writeln!(output).unwrap();
            }
        }
    }
    output
}
