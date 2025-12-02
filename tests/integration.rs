use std::{fs::read_to_string, io::Write};

use cargo_dequalify::process_file;
use tempfile::NamedTempFile;

fn process_source(src: &str, ignore_roots: &[String]) -> String {
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(src.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    process_file(&path, ignore_roots, false).unwrap();
    read_to_string(&path).unwrap()
}

#[test]
fn test_simple_rewrite() {
    let input = r#"
fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task::spawn;"));
    assert!(output.contains("spawn(async"));
    assert!(!output.contains("tokio::task::spawn(async"));
}

#[test]
fn test_multiple_paths() {
    let input = r#"
fn main() {
    tokio::task::spawn(async {});
    std::fs::read_to_string("foo");
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use std::fs::read_to_string;"));
    assert!(output.contains("use tokio::task::spawn;"));
}

#[test]
fn test_ignore_roots() {
    let input = r#"
fn main() {
    tokio::task::spawn(async {});
    std::fs::read_to_string("foo");
}
"#;
    let output = process_source(input, &["std".to_string()]);
    assert!(output.contains("use tokio::task::spawn;"));
    assert!(!output.contains("use std::fs::read_to_string;"));
    assert!(output.contains("std::fs::read_to_string"));
}

#[test]
fn test_rewrite_crate_self_super() {
    // Both paths have same short name (helper) and same parent (inner)
    // They conflict at all levels, so neither can be rewritten
    let input = r#"
mod inner {
    pub fn helper() {}
}
fn main() {
    crate::inner::helper();
    self::inner::helper();
}
"#;
    let output = process_source(input, &[]);
    // Both conflict at all levels (helper, inner), can't be resolved
    // They remain unchanged
    assert!(output.contains("crate::inner::helper()"));
    assert!(output.contains("self::inner::helper()"));
}

#[test]
fn test_skip_self_type() {
    let input = r#"
struct Foo;
impl Foo {
    fn new() -> Self {
        Self::default()
    }
    fn default() -> Self {
        Foo
    }
}
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(input.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    let changed = process_file(&path, &[], false).unwrap();
    assert!(changed.is_none());
}

#[test]
fn test_conflict_with_local_function() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
fn spawn() {}

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_conflict_with_existing_import() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
use other::spawn;

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_alias_on_conflict() {
    // With --alias-on-conflict, use parent module import instead of alias
    let input = r#"
fn spawn() {}

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[]);
    // Import parent module instead of aliasing
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_alias_conflict_with_import() {
    // With --alias-on-conflict, use parent module import instead of alias
    let input = r#"
use other::spawn;

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[]);
    // Import parent module instead of aliasing
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_no_changes_returns_false() {
    let input = r#"
fn main() {
    let x = 1 + 2;
}
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(input.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    let changed = process_file(&path, &[], false).unwrap();
    assert!(changed.is_none());
}

#[test]
fn test_preserves_existing_imports() {
    let input = r#"
use std::io::Write;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use std::io::Write;"));
    assert!(output.contains("use tokio::task::spawn;"));
}

#[test]
fn test_deep_nested_path() {
    let input = r#"
fn main() {
    a::b::c::d::e::func();
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use a::b::c::d::e::func;"));
    assert!(output.contains("func()"));
}

#[test]
fn test_multiple_calls_same_path() {
    let input = r#"
fn main() {
    tokio::task::spawn(async {});
    tokio::task::spawn(async {});
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    let import_count = output.matches("use tokio::task::spawn;").count();
    assert_eq!(import_count, 1);
    let spawn_calls = output.matches("spawn(async").count();
    assert_eq!(spawn_calls, 3);
}

#[test]
fn test_conflict_with_let_binding() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
fn main() {
    let spawn = 42;
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_conflict_with_struct() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
struct spawn;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_parent_module_on_multiple_conflicts() {
    // When function name conflicts, import parent module
    // When parent module also conflicts, go up another level
    // But if that results in same-as-original, it can't be resolved
    let input = r#"
fn spawn() {}
fn task() {}

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    // spawn conflicts, task conflicts, tokio would be same-as-original
    // Can't be resolved, stays unchanged
    assert!(output.contains("tokio::task::spawn(async"));
    assert!(!output.contains("use tokio;"));
}

#[test]
fn test_dry_run_does_not_modify() {
    let input = r#"
fn main() {
    tokio::task::spawn(async {});
}
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(input.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    let changed = process_file(&path, &[], true).unwrap();
    assert!(changed.is_some());
    let content = read_to_string(&path).unwrap();
    assert_eq!(content, input);
}

#[test]
fn test_nested_function_calls() {
    let input = r#"
fn main() {
    tokio::task::spawn(std::fs::read_to_string("foo"));
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use std::fs::read_to_string;"));
    assert!(output.contains("use tokio::task::spawn;"));
    assert!(output.contains("spawn(read_to_string"));
}

#[test]
fn test_method_call_not_affected() {
    let input = r#"
fn main() {
    let s = String::new();
    s.push_str("hello");
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("s.push_str"));
    assert!(output.contains("use tokio::task::spawn;"));
}

#[test]
fn test_conflict_with_enum() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
enum spawn { A, B }

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_conflict_with_type_alias() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
type spawn = i32;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_conflict_with_const() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
const spawn: i32 = 42;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_conflict_with_static() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
static spawn: i32 = 42;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_use_rename_conflict() {
    // With parent module import as default, spawn conflict causes use of task::spawn
    let input = r#"
use other::foo as spawn;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task;"));
    assert!(output.contains("task::spawn(async"));
}

#[test]
fn test_single_segment_path_ignored() {
    let input = r#"
fn foo() {}

fn main() {
    foo();
}
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(input.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    let changed = process_file(&path, &[], false).unwrap();
    assert!(changed.is_none());
}

#[test]
fn test_skip_type_associated_functions() {
    let input = r#"
fn main() {
    let v = Vec::new();
    let s = String::from("hello");
}
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(input.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    let changed = process_file(&path, &[], false).unwrap();
    assert!(changed.is_none());
}

#[test]
fn test_skip_nested_type_associated_functions() {
    // pdf2svg::Cli::try_new() - Cli is a type, should not be rewritten
    let input = r#"
fn main() {
    mycrate::MyType::new();
    foo::bar::Baz::create();
}
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(input.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    let changed = process_file(&path, &[], false).unwrap();
    assert!(changed.is_none());
}

#[test]
fn test_preserves_doc_comments() {
    let input = r#"
/// This is a doc comment
fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    // Verify doc comments are preserved as /// not #[doc = "..."]
    assert!(output.contains("/// This is a doc comment"));
    assert!(!output.contains("#[doc = "));
}

#[test]
fn test_unicode_before_path() {
    // Test that Unicode characters before the path don't break byte offset calculation
    let input = r#"
fn main() {
    let 日本語 = "hello";
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task::spawn;"));
    assert!(output.contains("spawn(async"));
    assert!(output.contains("let 日本語 = \"hello\";")); // Unicode preserved
}

#[test]
fn test_unicode_in_string_before_path() {
    let input = r#"
fn main() {
    let s = "日本語テスト";
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use tokio::task::spawn;"));
    assert!(output.contains("spawn(async"));
    assert!(output.contains("\"日本語テスト\"")); // Unicode string preserved
}

#[test]
fn test_conflict_between_new_imports() {
    // Multiple 2-segment paths with same short name can't be resolved
    // (at level 1, it would be same-as-original)
    let input = r#"
fn main() {
    module_a::handle();
    module_b::handle();
    module_c::handle();
}
"#;
    let output = process_source(input, &[]);
    // All three conflict on 'handle', can't go up further without being same-as-original
    // All remain unchanged
    assert!(output.contains("module_a::handle()"));
    assert!(output.contains("module_b::handle()"));
    assert!(output.contains("module_c::handle()"));
}

#[test]
fn test_conflict_between_new_imports_with_parent_module() {
    // Paths with same short name AND same parent name can't be resolved
    let input = r#"
fn main() {
    module_a::sub::handle();
    module_b::sub::handle();
    module_c::sub::handle();
}
"#;
    let output = process_source(input, &[]);
    // All three conflict on 'handle', all three conflict on 'sub'
    // Going to level 1 would be same-as-original, so can't be resolved
    assert!(output.contains("module_a::sub::handle()"));
    assert!(output.contains("module_b::sub::handle()"));
    assert!(output.contains("module_c::sub::handle()"));
}

#[test]
fn test_conflict_resolved_with_different_parents() {
    // Paths with same short name but DIFFERENT parent names can be resolved
    let input = r#"
fn main() {
    module_a::foo::handle();
    module_b::bar::handle();
    module_c::baz::handle();
}
"#;
    let output = process_source(input, &[]);
    // All three conflict on 'handle', so all go up to parent level
    // foo, bar, baz are all different, so no conflict
    assert!(output.contains("use module_a::foo;"));
    assert!(output.contains("use module_b::bar;"));
    assert!(output.contains("use module_c::baz;"));
    assert!(output.contains("foo::handle()"));
    assert!(output.contains("bar::handle()"));
    assert!(output.contains("baz::handle()"));
}

#[test]
fn test_preserves_trailing_comment_on_use() {
    // New imports should be inserted after the trailing comment, not in the middle of it
    let input = r#"
use crate::CompileParams; // For documentation purposes.

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    // The trailing comment should remain intact
    assert!(output.contains("use crate::CompileParams; // For documentation purposes."));
    assert!(output.contains("use tokio::task::spawn;"));
    // The new import should NOT be inserted in the middle of the comment
    // (the broken pattern would be "// For documentat\nuse tokio::task::spawn;")
    assert!(!output.contains("// For documentat\n"));
}

#[test]
fn test_preserves_trailing_comment_with_unicode() {
    // Ensure multi-byte UTF-8 characters in trailing comments don't break byte offset calculation
    let input = r#"
use crate::Foo; // 日本語コメント

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    // The trailing comment with Unicode should remain intact
    assert!(output.contains("use crate::Foo; // 日本語コメント"));
    assert!(output.contains("use tokio::task::spawn;"));
    // New imports should be on their own line, not breaking the comment
    assert!(!output.contains("日本語コメン\n"));
}

#[test]
fn test_qualified_macro_rewrite() {
    let input = r#"
fn main() {
    anyhow::bail!("error");
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use anyhow::bail;"));
    assert!(output.contains("bail!(\"error\")"));
    assert!(!output.contains("anyhow::bail!"));
}

#[test]
fn test_multiple_qualified_macros() {
    let input = r#"
fn main() {
    anyhow::bail!("error");
    tokio::select! {
        _ = async {} => {}
    }
}
"#;
    let output = process_source(input, &[]);
    assert!(output.contains("use anyhow::bail;"));
    assert!(output.contains("use tokio::select;"));
}

#[test]
fn test_macro_with_ignore_roots() {
    let input = r#"
fn main() {
    anyhow::bail!("error");
    std::println!("hello");
}
"#;
    let output = process_source(input, &["std".to_string()]);
    assert!(output.contains("use anyhow::bail;"));
    assert!(!output.contains("use std::println;"));
    assert!(output.contains("std::println!"));
}

#[test]
fn test_skip_primitive_type_methods() {
    // Primitive types like u32, i64, etc. cannot be imported via `use`
    // e.g., `use u32::try_from;` is invalid Rust
    let input = r#"
fn main() {
    let x = u32::try_from(42u64).unwrap();
    let y = u64::from(42u32);
    let z = i32::try_from(100i64).unwrap();
    let w = usize::try_from(10u32).unwrap();
    let f = f64::from(1.0f32);
}
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(input.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    let changed = process_file(&path, &[], false).unwrap();
    // No changes should be made - primitive type methods should be skipped
    assert!(changed.is_none());
}

#[test]
fn test_use_inside_mod() {
    // Use statements should be placed inside the module where the code is
    let input = r#"
mod tests {
    fn test_foo() {
        tokio::task::spawn(async {});
    }
}
"#;
    let output = process_source(input, &[]);
    // Use statement should be inside mod tests, not at file level
    assert!(output.contains("mod tests {\n    use tokio::task::spawn;"));
    assert!(output.contains("spawn(async"));
    // Use statement should NOT be at file level (before mod tests)
    assert!(!output.starts_with("\nuse tokio::task::spawn;"));
}

#[test]
fn test_use_inside_mod_with_existing_use() {
    // New use statements should be placed after existing uses in the module
    let input = r#"
mod tests {
    use std::time::Instant;

    fn test_foo() {
        tokio::task::spawn(async {});
    }
}
"#;
    let output = process_source(input, &[]);
    // Use statement should be inside mod tests after the existing use
    assert!(output.contains("use std::time::Instant;\n    use tokio::task::spawn;"));
    assert!(output.contains("spawn(async"));
}

#[test]
fn test_use_in_nested_mod() {
    // Use statements should be placed in the innermost containing module
    let input = r#"
mod outer {
    mod inner {
        fn test_foo() {
            tokio::task::spawn(async {});
        }
    }
}
"#;
    let output = process_source(input, &[]);
    // Use statement should be inside mod inner
    assert!(output.contains("mod inner {\n        use tokio::task::spawn;"));
}

#[test]
fn test_multiple_mods_separate_uses() {
    // Each module should get its own use statements
    let input = r#"
mod mod_a {
    fn foo() {
        tokio::task::spawn(async {});
    }
}

mod mod_b {
    fn bar() {
        std::fs::read_to_string("x");
    }
}
"#;
    let output = process_source(input, &[]);
    // mod_a should have spawn import
    assert!(output.contains("mod mod_a {\n    use tokio::task::spawn;"));
    // mod_b should have read_to_string import
    assert!(output.contains("mod mod_b {\n    use std::fs::read_to_string;"));
}

#[test]
fn test_file_level_and_mod_level() {
    // Uses at file level should stay at file level, mod level should be in mod
    let input = r#"
fn main() {
    std::fs::read_to_string("x");
}

mod tests {
    fn test_foo() {
        tokio::task::spawn(async {});
    }
}
"#;
    let output = process_source(input, &[]);
    // File level should have read_to_string
    assert!(output.contains("\nuse std::fs::read_to_string;\n"));
    // mod tests should have spawn (not at file level)
    assert!(output.contains("mod tests {\n    use tokio::task::spawn;"));
}

#[test]
fn test_nested_mods_with_calls_at_each_level() {
    // Test multiple nested modules with calls at each level
    let input = r#"
mod outer {
    fn outer_fn() {
        tokio::task::spawn(async {});
    }

    mod inner {
        fn inner_fn() {
            std::fs::read_to_string("x");
        }

        mod deepest {
            fn deepest_fn() {
                anyhow::bail!("error");
            }
        }
    }
}
"#;
    let output = process_source(input, &[]);
    // outer should have spawn
    assert!(output.contains("mod outer {\n    use tokio::task::spawn;"));
    // inner should have read_to_string
    assert!(output.contains("mod inner {\n        use std::fs::read_to_string;"));
    // deepest should have bail
    assert!(output.contains("mod deepest {\n            use anyhow::bail;"));
}

#[test]
fn test_same_import_in_different_modules() {
    // Same path used in different modules should get imports in each module
    let input = r#"
mod mod_a {
    fn foo() {
        tokio::task::spawn(async {});
    }
}

mod mod_b {
    fn bar() {
        tokio::task::spawn(async {});
    }
}
"#;
    let output = process_source(input, &[]);
    // Both modules should have their own spawn import
    assert!(output.contains("mod mod_a {\n    use tokio::task::spawn;"));
    assert!(output.contains("mod mod_b {\n    use tokio::task::spawn;"));
}

#[test]
fn test_mod_in_middle_of_file() {
    // Mod can appear anywhere in a file - code before and after should work
    let input = r#"
fn before_mod() {
    tokio::task::spawn(async {});
}

mod middle {
    fn inside_mod() {
        anyhow::bail!("error");
    }
}

fn after_mod() {
    std::fs::read_to_string("x");
}
"#;
    let output = process_source(input, &[]);
    // File-level uses should be at the top
    assert!(output.contains("use std::fs::read_to_string;"));
    assert!(output.contains("use tokio::task::spawn;"));
    // Module use should be inside the module
    assert!(output.contains("mod middle {\n    use anyhow::bail;"));
    // All calls should be dequalified
    assert!(output.contains("spawn(async"));
    assert!(output.contains("bail!(\"error\")"));
    assert!(output.contains("read_to_string(\"x\")"));
}

#[test]
fn test_use_inside_function_expanded() {
    // `use` inside a function creates a local alias - paths using that alias
    // are now expanded and dequalified.
    // e.g., `use tokio::task;` and `task::spawn()` -> expands to `tokio::task::spawn`
    let input = r#"
fn some_function() {
    use tokio::task;
    task::spawn(async {});
}
"#;
    let output = process_source(input, &[]);
    // Should add use for the expanded path
    assert!(output.contains("use tokio::task::spawn;"));
    // task::spawn should be replaced with spawn
    assert!(output.contains("spawn(async"));
    // The original local import remains (we don't remove it)
    assert!(output.contains("use tokio::task;"));
}

#[test]
fn test_mixed_local_import_and_crate_path() {
    // If there's a local import and a separate crate path, both should be dequalified
    let input = r#"
fn some_function() {
    use tokio::task;
    task::spawn(async {});  // expands to tokio::task::spawn, then dequalified
    std::fs::read_to_string("x");  // crate path, dequalify
}
"#;
    let output = process_source(input, &[]);
    // Should have use for both
    assert!(output.contains("use std::fs::read_to_string;"));
    assert!(output.contains("use tokio::task::spawn;"));
    // Both should be dequalified
    assert!(output.contains("spawn(async"));
    assert!(output.contains("read_to_string(\"x\")"));
}
