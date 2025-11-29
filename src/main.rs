mod rewrite;

use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{Context, Result};
use clap::Parser;
use dunce::canonicalize;
use fs::read_to_string;
use ignore::WalkBuilder;
use rewrite::process_file;
use serde::Deserialize;
use std::env::args;
use toml::from_str;

/// cargo-dequalify
///
/// Rewrite fully-qualified function calls into imported short names.
///
/// Examples:
///   cargo dequalify
///   cargo dequalify --alias-on-conflict
///
/// By default:
///   - Runs on the current workspace (or single crate)
///   - Rewrites only when there is no name conflict
///   - If the short name already exists (import/local), it skips and prints a warning
///
/// With --alias-on-conflict:
///   - On conflict, generates an alias: use tokio::task::spawn as tokio_task_spawn;
///     tokio_task_spawn(foo());
#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Optional path to a package or workspace root. Defaults to current dir.
    #[arg(value_name = "PATH", default_value = ".")]
    target: PathBuf,

    /// Actually modify files (default: dry-run mode).
    #[arg(short, long)]
    write: bool,

    /// Comma-separated list of top-level roots to ignore (e.g. "std,core,alloc").
    #[arg(long, value_delimiter = ',')]
    ignore_roots: Vec<String>,

    /// When a short name would conflict, import with an alias and rewrite calls.
    /// Example:
    ///   tokio::task::spawn(foo())
    ///   =>
    ///   use tokio::task::spawn as tokio_task_spawn;
    ///   tokio_task_spawn(foo());
    #[arg(long)]
    alias_on_conflict: bool,

    /// Run cargo fmt after writing changes. Optionally specify a toolchain (e.g., --fmt=nightly).
    #[arg(short, long, value_name = "TOOLCHAIN")]
    fmt: Option<Option<String>>,
}

fn main() -> Result<()> {
    // Normalize args so it works both as:
    //   - cargo dequalify
    //   - cargo-dequalify (called directly)
    let mut raw_args: Vec<String> = args().collect();

    // raw_args[0] = "cargo-dequalify" (binary name)
    // raw_args[1] might be "dequalify" when invoked as `cargo dequalify`
    if raw_args.get(1).map(String::as_str) == Some("dequalify") {
        // Remove the cargo-injected subcommand name
        raw_args.remove(1);
    }

    let cli = Cli::parse_from(&raw_args);

    let root = canonicalize(&cli.target)
        .with_context(|| format!("failed to canonicalize path {}", cli.target.display()))?;

    // Find Cargo.toml starting from the target path (walk up)
    let cargo_toml_path = find_cargo_toml(&root)
        .with_context(|| format!("failed to find Cargo.toml starting from {}", root.display()))?;

    let workspace = load_workspace(&cargo_toml_path)
        .with_context(|| format!("failed to load workspace from {}", cargo_toml_path.display()))?;

    let crate_roots = workspace_crate_roots(&cargo_toml_path, &workspace);

    let mut any_changes = false;

    if crate_roots.is_empty() {
        // Fall back to treating the directory with Cargo.toml as a single crate.
        let crate_root = cargo_toml_path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf();
        any_changes |= process_crate(&crate_root, &cli)?;
    } else {
        for crate_root in crate_roots {
            any_changes |= process_crate(&crate_root, &cli)?;
        }
    }

    if any_changes && !cli.write {
        eprintln!("# Run with `-w` to apply changes, or `-w -f` to also format.");
    }

    if any_changes
        && cli.write
        && let Some(toolchain) = &cli.fmt
    {
        run_cargo_fmt(toolchain.as_deref())?;
    }

    Ok(())
}

fn run_cargo_fmt(toolchain: Option<&str>) -> Result<()> {
    let mut cmd = Command::new("cargo");
    if let Some(tc) = toolchain {
        cmd.arg(format!("+{}", tc));
    }
    cmd.arg("fmt");

    let status = cmd.status().context("failed to run cargo fmt")?;
    if !status.success() {
        anyhow::bail!("cargo fmt failed with {}", status);
    }
    Ok(())
}

/// Represents just enough of Cargo.toml for our purposes.
#[derive(Debug, Deserialize)]
struct CargoWorkspaceToml {
    workspace: Option<WorkspaceSection>,
    package: Option<PackageSection>,
}

#[derive(Debug, Deserialize)]
struct WorkspaceSection {
    members: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
struct PackageSection {}

/// Information about the workspace: where it lives and what crates it contains.
#[derive(Debug)]
struct WorkspaceInfo {
    /// True if this is a virtual workspace (no [package] at root)
    virtual_root: bool,
    /// Members as relative paths from workspace root.
    members: Vec<String>,
}

/// Walk up from `start` to find a Cargo.toml.
fn find_cargo_toml(start: &Path) -> Result<PathBuf> {
    let mut dir = start;

    loop {
        let candidate = dir.join("Cargo.toml");
        if candidate.is_file() {
            return Ok(candidate);
        }

        match dir.parent() {
            Some(parent) => {
                dir = parent;
            }
            None => break,
        }
    }

    anyhow::bail!("Cargo.toml not found");
}

/// Load workspace / package info from Cargo.toml.
fn load_workspace(cargo_toml: &Path) -> Result<WorkspaceInfo> {
    let content = read_to_string(cargo_toml)
        .with_context(|| format!("failed to read {}", cargo_toml.display()))?;
    let parsed: CargoWorkspaceToml = from_str(&content)
        .with_context(|| format!("failed to parse TOML {}", cargo_toml.display()))?;

    let virtual_root = parsed.package.is_none();

    let members = if let Some(ws) = parsed.workspace {
        let mut m = ws.members.unwrap_or_default();
        // If default-members specified, you might want to use them instead;
        // for now we'll just use members.
        m.sort();
        m.dedup();
        m
    } else if !virtual_root {
        // Non-workspace single crate: no members; will be handled as root crate.
        Vec::new()
    } else {
        Vec::new()
    };

    Ok(WorkspaceInfo { virtual_root, members })
}

/// Resolve crate roots from workspace info.
fn workspace_crate_roots(cargo_toml: &Path, ws: &WorkspaceInfo) -> Vec<PathBuf> {
    let root_dir = cargo_toml.parent().unwrap_or_else(|| Path::new("."));
    let mut roots = Vec::new();

    // If there are explicit members, use them.
    for member in &ws.members {
        roots.push(root_dir.join(member));
    }

    // If root is a non-virtual crate (has [package]), include it as well.
    if !ws.virtual_root {
        roots.push(root_dir.to_path_buf());
    }

    roots.sort();
    roots.dedup();
    roots
}

/// Process all .rs files under a crate root, honoring .gitignore (global and local).
fn process_crate(crate_root: &Path, cli: &Cli) -> Result<bool> {
    // Build a walker that:
    // - Starts at crate_root
    // - Respects .gitignore, .git/info/exclude, global gitignore
    // - Skips VCS ignores only if explicitly configured (we keep defaults)
    // - Follows ignore rules by default
    let mut builder = WalkBuilder::new(crate_root);
    builder
        .standard_filters(true) // ignore .git, target, etc. via .gitignore and defaults
        .hidden(false) // let .gitignore decide; not "hidden" heuristic
        .follow_links(false); // avoid symlink surprises in workspaces

    let walker = builder.build();

    let mut any_changes = false;

    for result in walker {
        let entry = match result {
            Ok(e) => e,
            Err(err) => {
                eprintln!("walk error under {}: {err}", crate_root.display());
                continue;
            }
        };

        let path = entry.path();

        if !path.is_file() {
            continue;
        }

        if path.extension() != Some(OsStr::new("rs")) {
            continue;
        }

        any_changes |= process_file(path, &cli.ignore_roots, !cli.write, cli.alias_on_conflict)?;
    }

    Ok(any_changes)
}
