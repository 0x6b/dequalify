# dequalify

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

```sh
cargo install --git https://github.com/0x6b/dequalify
```

## Usage

```sh
# Preview changes (default, dry-run mode)
dequalify src/main.rs

# Process a directory recursively
dequalify src/

# Actually modify files
dequalify --write src/
dequalify -w src/

# Verbose output
dequalify --verbose src/

# Ignore specific root modules (e.g., std, core)
dequalify --ignore-roots std,core src/

# Use aliases when names conflict
dequalify --alias-on-conflict src/
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
