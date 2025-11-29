use std::{fs::read_to_string, io::Write};

use cargo_dequalify::process_file;
use tempfile::NamedTempFile;

fn process_source(src: &str, ignore_roots: &[String], alias_on_conflict: bool) -> String {
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(src.as_bytes()).unwrap();
    let path = file.path().to_path_buf();
    process_file(&path, ignore_roots, false, alias_on_conflict).unwrap();
    read_to_string(&path).unwrap()
}

#[test]
fn test_simple_rewrite() {
    let input = r#"
fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
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
    let output = process_source(input, &[], false);
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
    let output = process_source(input, &["std".to_string()], false);
    assert!(output.contains("use tokio::task::spawn;"));
    assert!(!output.contains("use std::fs::read_to_string;"));
    assert!(output.contains("std::fs::read_to_string"));
}

#[test]
fn test_rewrite_crate_self_super() {
    let input = r#"
mod inner {
    pub fn helper() {}
}
fn main() {
    crate::inner::helper();
    self::inner::helper();
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("use crate::inner::helper;"));
    assert!(output.contains("use self::inner::helper;"));
    assert!(output.contains("helper()"));
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
    let changed = process_file(&path, &[], false, false).unwrap();
    assert!(!changed);
}

#[test]
fn test_conflict_with_local_function() {
    let input = r#"
fn spawn() {}

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
    assert!(!output.contains("use tokio::task::spawn;"));
}

#[test]
fn test_conflict_with_existing_import() {
    let input = r#"
use other::spawn;

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
    assert!(!output.contains("use tokio::task::spawn;"));
}

#[test]
fn test_alias_on_conflict() {
    let input = r#"
fn spawn() {}

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[], true);
    assert!(output.contains("use tokio::task::spawn as tokio_task_spawn;"));
    assert!(output.contains("tokio_task_spawn(async"));
}

#[test]
fn test_alias_conflict_with_import() {
    let input = r#"
use other::spawn;

fn main() {
    tokio::task::spawn(async {});
    spawn();
}
"#;
    let output = process_source(input, &[], true);
    assert!(output.contains("use tokio::task::spawn as tokio_task_spawn;"));
    assert!(output.contains("tokio_task_spawn(async"));
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
    let changed = process_file(&path, &[], false, false).unwrap();
    assert!(!changed);
}

#[test]
fn test_preserves_existing_imports() {
    let input = r#"
use std::io::Write;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
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
    let output = process_source(input, &[], false);
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
    let output = process_source(input, &[], false);
    let import_count = output.matches("use tokio::task::spawn;").count();
    assert_eq!(import_count, 1);
    let spawn_calls = output.matches("spawn(async").count();
    assert_eq!(spawn_calls, 3);
}

#[test]
fn test_conflict_with_let_binding() {
    let input = r#"
fn main() {
    let spawn = 42;
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
}

#[test]
fn test_conflict_with_struct() {
    let input = r#"
struct spawn;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
}

#[test]
fn test_alias_numbering_on_multiple_conflicts() {
    let input = r#"
fn spawn() {}
fn tokio_task_spawn() {}

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], true);
    assert!(output.contains("use tokio::task::spawn as tokio_task_spawn_1;"));
    assert!(output.contains("tokio_task_spawn_1(async"));
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
    let changed = process_file(&path, &[], true, false).unwrap();
    assert!(changed);
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
    let output = process_source(input, &[], false);
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
    let output = process_source(input, &[], false);
    assert!(output.contains("s.push_str"));
    assert!(output.contains("use tokio::task::spawn;"));
}

#[test]
fn test_conflict_with_enum() {
    let input = r#"
enum spawn { A, B }

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
}

#[test]
fn test_conflict_with_type_alias() {
    let input = r#"
type spawn = i32;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
}

#[test]
fn test_conflict_with_const() {
    let input = r#"
const spawn: i32 = 42;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
}

#[test]
fn test_conflict_with_static() {
    let input = r#"
static spawn: i32 = 42;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
}

#[test]
fn test_use_rename_conflict() {
    let input = r#"
use other::foo as spawn;

fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
    assert!(output.contains("tokio::task::spawn(async"));
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
    let changed = process_file(&path, &[], false, false).unwrap();
    assert!(!changed);
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
    let changed = process_file(&path, &[], false, false).unwrap();
    assert!(!changed);
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
    let changed = process_file(&path, &[], false, false).unwrap();
    assert!(!changed);
}

#[test]
fn test_preserves_doc_comments() {
    let input = r#"
/// This is a doc comment
fn main() {
    tokio::task::spawn(async {});
}
"#;
    let output = process_source(input, &[], false);
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
    let output = process_source(input, &[], false);
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
    let output = process_source(input, &[], false);
    assert!(output.contains("use tokio::task::spawn;"));
    assert!(output.contains("spawn(async"));
    assert!(output.contains("\"日本語テスト\"")); // Unicode string preserved
}
