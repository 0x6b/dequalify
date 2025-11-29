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
    last_ident: String,
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
                    let last_segment = segments.last().unwrap();
                    let last_ident = last_segment.ident.to_string();
                    let full_str = path_to_string(path);

                    // Get span info for the entire path expression
                    let span = expr_path.span();
                    let start = span.start();
                    let end = span.end();

                    self.paths
                        .entry(full_str.clone())
                        .or_insert(PathInfo { last_ident: last_ident.clone(), line: start.line });

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

// Manages conflict detection and alias generation.
struct ConflictResolver {
    imported_idents: BTreeSet<String>,
    local_idents: BTreeSet<String>,
    aliases: BTreeMap<String, String>,
}

impl ConflictResolver {
    fn new(imported_idents: BTreeSet<String>, local_idents: BTreeSet<String>) -> Self {
        Self {
            imported_idents,
            local_idents,
            aliases: BTreeMap::new(),
        }
    }

    fn has_conflict(&self, ident: &str) -> bool {
        self.imported_idents.contains(ident) || self.local_idents.contains(ident)
    }

    fn is_used(&self, name: &str) -> bool {
        self.imported_idents.contains(name)
            || self.local_idents.contains(name)
            || self.aliases.values().any(|v| v == name)
    }

    // Get or create an alias for a conflicting full path.
    fn alias_for(&mut self, full_path_str: &str) -> String {
        if let Some(alias) = self.aliases.get(full_path_str) {
            return alias.clone();
        }
        let base = full_path_str.replace("::", "_");
        let mut candidate = base.clone();
        let mut idx = 1;
        while self.is_used(&candidate) {
            candidate = format!("{base}_{idx}");
            idx += 1;
        }
        self.aliases.insert(full_path_str.to_string(), candidate.clone());
        candidate
    }
}

// A text replacement to apply.
#[derive(Debug)]
struct Replacement {
    start: usize,
    end: usize,
    new_text: String,
}

// Process a single file.
// Returns true if the file would be / was changed.
pub fn process_file(
    path: &Path,
    ignore_roots: &[String],
    dry_run: bool,
    alias_on_conflict: bool,
) -> Result<bool> {
    let src = read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    let ast: File =
        parse_file(&src).with_context(|| format!("failed to parse {}", path.display()))?;

    let ignore_set: BTreeSet<String> = ignore_roots.iter().cloned().collect();
    let mut collector = PathCollector::new(&ignore_set);
    collector.visit_file(&ast);

    if collector.paths.is_empty() {
        return Ok(false);
    }

    let imported_idents = collect_imported_idents(&ast);
    let local_idents = collect_local_idents(&ast);
    let mut resolver = ConflictResolver::new(imported_idents.clone(), local_idents.clone());

    let file_path_str = path.display().to_string();
    let line_offsets = LineOffsets::new(&src);

    // Group occurrences by path for efficient lookup
    let mut occ_by_path: BTreeMap<&str, Vec<&PathOccurrence>> = BTreeMap::new();
    for occ in &collector.occurrences {
        occ_by_path.entry(&occ.full_path_str).or_default().push(occ);
    }

    // Build replacements for each occurrence
    let mut replacements: Vec<Replacement> = Vec::new();
    let mut use_statements: Vec<String> = Vec::new();

    for (full_path_str, info) in &collector.paths {
        let has_conflict = resolver.has_conflict(&info.last_ident);

        if has_conflict && !alias_on_conflict {
            eprintln!(
                "conflict in {}:{}: identifier `{}` is already defined/imported; \
                 skipping rewrite of `{}`",
                file_path_str, info.line, info.last_ident, full_path_str
            );
            continue;
        }

        let replacement_text = if has_conflict {
            let alias = resolver.alias_for(full_path_str);
            use_statements.push(format!("use {} as {};", full_path_str, alias));
            alias
        } else {
            use_statements.push(format!("use {};", full_path_str));
            info.last_ident.clone()
        };

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
        return Ok(false);
    }

    // Sort replacements by start position descending (so we can apply from end to start)
    replacements.sort_by(|a, b| b.start.cmp(&a.start));

    // Apply replacements to source
    let mut new_src = src.clone();
    for repl in &replacements {
        new_src.replace_range(repl.start..repl.end, &repl.new_text);
    }

    // Insert use statements
    if !use_statements.is_empty() {
        use_statements.sort();
        let use_block = "\n".to_string() + &use_statements.join("\n") + "\n";
        let insert_pos = find_use_insert_position(&ast, &line_offsets);
        new_src.insert_str(insert_pos, &use_block);
    }

    if new_src == src {
        return Ok(false);
    }

    if dry_run {
        print_diff(path, &src, &new_src);
        return Ok(true);
    }

    write(path, &new_src).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(true)
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
}

// Find the byte position where use statements should be inserted.
fn find_use_insert_position(ast: &File, line_offsets: &LineOffsets) -> usize {
    let mut last_use_end: Option<usize> = None;

    for item in &ast.items {
        if let Item::Use(item_use) = item {
            let span = item_use.span();
            let end_line = span.end().line;
            let end_col = span.end().column;
            if let Some(byte_pos) = line_offsets.line_col_to_byte(end_line, end_col) {
                last_use_end = Some(byte_pos);
            }
        }
    }

    // Return the position after the last use statement, or 0 if none exist
    last_use_end.unwrap_or(0)
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

fn print_diff(path: &Path, old: &str, new: &str) {
    let diff = TextDiff::from_lines(old, new);
    println!("--- {}", path.display());
    println!("+++ {}", path.display());
    for hunk in diff.unified_diff().context_radius(3).iter_hunks() {
        println!("{}", hunk.header());
        for change in hunk.iter_changes() {
            let sign = match change.tag() {
                ChangeTag::Delete => "-",
                ChangeTag::Insert => "+",
                ChangeTag::Equal => " ",
            };
            print!("{}{}", sign, change);
            if change.missing_newline() {
                println!();
            }
        }
    }
}
