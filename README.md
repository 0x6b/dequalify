# cargo-dequalify

A Rust refactoring tool that rewrites fully-qualified function calls into imported short names.

Before:

```rust
fn main() {
    tokio::task::spawn(async {});
    std::fs::read_to_string("foo");
}
```

After:

```rust
use std::fs::read_to_string;
use tokio::task::spawn;

fn main() {
    spawn(async {});
    read_to_string("foo");
}
```

## Installation

```console
$ cargo install --git https://github.com/0x6b/cargo-dequalify
```

## Usage

```console
$ cargo dequalify --help
Rewrite fully-qualified Rust call paths into imported short names

Usage: cargo-dequalify [OPTIONS] [PATH]

Arguments:
  [PATH]  Optional path to a package or workspace root. Defaults to current dir [default: .]

Options:
  -w, --write                        Actually modify files (default: dry-run mode)
      --ignore-roots <IGNORE_ROOTS>  Comma-separated list of top-level roots to ignore (e.g. "std,core,alloc")
      --alias-on-conflict            When a short name would conflict, import with an alias and rewrite calls.
                                     Example: tokio::task::spawn(foo()) => use tokio::task::spawn as
                                     tokio_task_spawn; tokio_task_spawn(foo());
  -f, --fmt [<TOOLCHAIN>]            Run cargo fmt after writing changes. Optionally specify a toolchain (e.g.,
                                     --fmt=nightly)
  -h, --help                         Print help
  -V, --version                      Print version
```

## Conflict Handling

By default, if the short name would conflict with an existing import or local definition, the path is left unchanged and a warning is printed.

With `--alias-on-conflict`, conflicting paths are imported with an alias:

```rust
fn spawn() {}

fn main() {
    tokio::task::spawn(async {});
}
```

Becomes:

```rust
use tokio::task::spawn as tokio_task_spawn;

fn spawn() {}

fn main() {
    tokio_task_spawn(async {});
}
```

## Limitations

- Only rewrites function calls, not type paths
- Skips paths starting with uppercase (type-associated functions like `Vec::new()`)
- Skips `Self::` paths

## License

MIT. See [LICENSE](LICENSE) for detail.
