use std::{
    collections::{BTreeMap, BTreeSet},
    fs::{read_to_string, write},
    path::Path,
};

use anyhow::{Context, Result};
use similar::{ChangeTag, TextDiff};
use syn::{
    ExprCall, File, Item, ItemConst, ItemEnum, ItemFn, ItemImpl, ItemMod, ItemStatic, ItemStruct,
    ItemTrait, ItemType, ItemUnion, Pat, Path as SynPath, UseTree, parse_file,
    spanned::Spanned,
    visit::{
        Visit, {self},
    },
    visit_mut::{
        VisitMut, {self},
    },
};
use visit_mut::{visit_local_mut, visit_pat_ident_mut};

// Information about a fully-qualified function path we want to shorten.
#[derive(Clone)]
struct PathInfo {
    line: usize,
}

// A single occurrence of a path in the source that needs rewriting.
#[derive(Clone, Debug)]
struct PathOccurrence {
    full_path_str: String,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
}

// Collect all candidate fully-qualified paths from function calls.
struct PathCollector<'a> {
    paths: BTreeMap<String, PathInfo>,
    occurrences: Vec<PathOccurrence>,
    ignore_roots: &'a BTreeSet<String>,
}

impl<'a> PathCollector<'a> {
    fn new(ignore_roots: &'a BTreeSet<String>) -> Self {
        Self {
            paths: BTreeMap::new(),
            occurrences: Vec::new(),
            ignore_roots,
        }
    }
}

impl<'a> Visit<'_> for PathCollector<'a> {
    fn visit_expr_call(&mut self, node: &ExprCall) {
        if let syn::Expr::Path(expr_path) = &*node.func
            && expr_path.qself.is_none()
        {
            let path = &expr_path.path;
            let segments = &path.segments;
            if segments.len() >= 2 {
                let first_ident = &segments[0].ident;
                let first_name = first_ident.to_string();
                let first_char = first_name.chars().next().unwrap_or('a');

                // Check if second-to-last segment is uppercase (type associated function)
                // e.g., pdf2svg::Cli::try_new() - Cli is the type
                let second_to_last = &segments[segments.len() - 2].ident.to_string();
                let second_to_last_char = second_to_last.chars().next().unwrap_or('a');

                if first_name != "Self"
                    && !first_char.is_uppercase()
                    && !second_to_last_char.is_uppercase()
                    && !self.ignore_roots.contains(&first_name)
                {
                    let full_str = path_to_string(path);

                    // Get span info for the entire path expression
                    let span = expr_path.span();
                    let start = span.start();
                    let end = span.end();

                    self.paths
                        .entry(full_str.clone())
                        .or_insert(PathInfo { line: start.line });

                    self.occurrences.push(PathOccurrence {
                        full_path_str: full_str,
                        start_line: start.line,
                        start_col: start.column,
                        end_line: end.line,
                        end_col: end.column,
                    });
                }
            }
        }
        visit::visit_expr_call(self, node);
    }
}

/// Represents a path's import strategy at a given level.
/// `import_len` is how many segments to import (from the start).
/// For path `a::b::c::func` with 4 segments:
/// - import_len=4: `use a::b::c::func;` → `func()`
/// - import_len=3: `use a::b::c;` → `c::func()`
/// - import_len=2: `use a::b;` → `b::c::func()`
/// - import_len=1: `use a;` → `a::b::c::func()` (same as original, skip)
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
    let mut strategies: Vec<ImportStrategy> = paths.iter().map(|p| ImportStrategy::new(p)).collect();

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

// A text replacement to apply.
#[derive(Debug)]
struct Replacement {
    start: usize,
    end: usize,
    new_text: String,
}

// Process a single file.
// Returns Some(diff_string) if the file would be / was changed, None otherwise.
// In write mode, the diff string is empty but Some indicates changes occurred.
pub fn process_file(path: &Path, ignore_roots: &[String], dry_run: bool) -> Result<Option<String>> {
    let src = read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    let ast: File =
        parse_file(&src).with_context(|| format!("failed to parse {}", path.display()))?;

    let ignore_set: BTreeSet<String> = ignore_roots.iter().cloned().collect();
    let mut collector = PathCollector::new(&ignore_set);
    collector.visit_file(&ast);

    if collector.paths.is_empty() {
        return Ok(None);
    }

    let imported_idents = collect_imported_idents(&ast);
    let local_idents = collect_local_idents(&ast);
    let existing_idents: BTreeSet<String> = imported_idents
        .union(&local_idents)
        .cloned()
        .collect();

    let file_path_str = path.display().to_string();
    let line_offsets = LineOffsets::new(&src);

    // Group occurrences by path for efficient lookup
    let mut occ_by_path: BTreeMap<&str, Vec<&PathOccurrence>> = BTreeMap::new();
    for occ in &collector.occurrences {
        occ_by_path.entry(&occ.full_path_str).or_default().push(occ);
    }

    // Collect all unique paths
    let all_paths: Vec<String> = collector.paths.keys().cloned().collect();

    // Resolve all imports using multi-pass algorithm
    let strategies = resolve_all_imports(&all_paths, &existing_idents);

    // Build replacements for each occurrence
    let mut replacements: Vec<Replacement> = Vec::new();
    let mut use_statements: Vec<String> = Vec::new();

    for (full_path_str, info) in &collector.paths {
        let Some(strategy) = strategies.get(full_path_str) else {
            // Path couldn't be resolved (all levels conflict)
            eprintln!(
                "conflict in {}:{}: cannot resolve `{}` without conflicts at any level",
                file_path_str, info.line, full_path_str
            );
            continue;
        };

        use_statements.push(strategy.use_statement());
        let replacement_text = strategy.replacement();

        // Create replacements for all occurrences of this path
        if let Some(occurrences) = occ_by_path.get(full_path_str.as_str()) {
            for occ in occurrences {
                let start_byte = line_offsets.line_col_to_byte(occ.start_line, occ.start_col);
                let end_byte = line_offsets.line_col_to_byte(occ.end_line, occ.end_col);

                if let (Some(start), Some(end)) = (start_byte, end_byte) {
                    replacements.push(Replacement {
                        start,
                        end,
                        new_text: replacement_text.clone(),
                    });
                }
            }
        }
    }

    if replacements.is_empty() && use_statements.is_empty() {
        return Ok(None);
    }

    let mut new_src = src.clone();

    // Insert use statements FIRST, before applying replacements.
    // This is important because insert_pos is calculated from the original source,
    // and replacements would shift byte positions.
    let use_block_len = if !use_statements.is_empty() {
        use_statements.sort();
        let use_block = "\n".to_string() + &use_statements.join("\n") + "\n";
        let insert_pos = find_use_insert_position(&ast, &line_offsets);
        new_src.insert_str(insert_pos, &use_block);
        (insert_pos, use_block.len())
    } else {
        (0, 0)
    };

    // Sort replacements by start position descending (so we can apply from end to start)
    replacements.sort_by(|a, b| b.start.cmp(&a.start));

    // Apply replacements to source, adjusting offsets for the inserted use block
    let (use_insert_pos, use_len) = use_block_len;
    for repl in &replacements {
        let adjusted_start = if repl.start >= use_insert_pos {
            repl.start + use_len
        } else {
            repl.start
        };
        let adjusted_end = if repl.end >= use_insert_pos {
            repl.end + use_len
        } else {
            repl.end
        };
        new_src.replace_range(adjusted_start..adjusted_end, &repl.new_text);
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
            return Some(line_start + char_offsets.last().map(|&o| o + 1).unwrap_or(0));
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
            line_start + char_offsets.last().map(|&o| o + 1).unwrap_or(0)
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
    if let Some(line) = last_use_line {
        line_offsets.line_end_byte(line)
    } else {
        0
    }
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
        fn visit_local_mut(&mut self, node: &mut syn::Local) {
            collect_pattern_idents(&node.pat, self.set);
            visit_local_mut(self, node);
        }
        fn visit_pat_ident_mut(&mut self, node: &mut syn::PatIdent) {
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
            write!(output, "{}{}", sign, change).unwrap();
            if change.missing_newline() {
                writeln!(output).unwrap();
            }
        }
    }
    output
}
