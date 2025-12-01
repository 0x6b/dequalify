mod rewrite;

use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    process::Command,
    sync::{
        Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use anyhow::{Context, Result};
use clap::Parser;
use dunce::canonicalize;
use rayon::prelude::*;
use fs::read_to_string;
use ignore::WalkBuilder;
use rewrite::process_file;
use serde::Deserialize;
use std::env::args;
use toml::from_str;
use gix::discover;
use anyhow::bail;



/// cargo-dequalify
///
/// Rewrite fully-qualified function calls into imported short names.
///
/// Examples:
///   cargo dequalify
///   cargo dequalify -w
///
/// By default:
///   - Runs on the current workspace (or single crate)
///   - Uses dry-run mode (use -w to write changes)
///   - On conflict, imports the parent module instead
///     (e.g., `tokio::task::spawn` → use `tokio::task`; `task::spawn()`)
#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Optional path to a package or workspace root. Defaults to current dir.
    #[arg(value_name = "PATH", default_value = ".")]
    target: PathBuf,

    /// Actually modify files (default: dry-run mode).
    #[arg(short, long)]
    write: bool,

    /// Allow running --write on a dirty git working directory.
    #[arg(long)]
    allow_dirty: bool,

    /// Comma-separated list of top-level roots to ignore (e.g. "std,core,alloc").
    #[arg(long, value_delimiter = ',')]
    ignore_roots: Vec<String>,

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

    // Check for dirty git working directory when --write is used
    if cli.write && !cli.allow_dirty && is_git_dirty(&root) {
        bail!(
            "working directory has uncommitted changes, \
             please commit or stash them before running with --write, \
             or use --allow-dirty to override"
        );
    }

    let mut crate_roots = workspace_crate_roots(&cargo_toml_path, &workspace);

    if crate_roots.is_empty() {
        // Fall back to treating the directory with Cargo.toml as a single crate.
        let crate_root = cargo_toml_path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf();
        crate_roots.push(crate_root);
    }

    // Collect all .rs files from all crates
    let mut rs_files: Vec<PathBuf> = Vec::new();
    for crate_root in &crate_roots {
        let walker = WalkBuilder::new(crate_root)
            .standard_filters(true)
            .hidden(false)
            .follow_links(false)
            .build();

        for entry in walker.filter_map(std::result::Result::ok) {
            let path = entry.path();
            if path.is_file() && path.extension() == Some(OsStr::new("rs")) {
                rs_files.push(path.to_path_buf());
            }
        }
    }

    let any_changes = AtomicBool::new(false);
    let diffs: Mutex<Vec<(PathBuf, String)>> = Mutex::new(Vec::new());

    // Process all files in parallel
    rs_files.par_iter().for_each(|path| {
        match process_file(path, &cli.ignore_roots, !cli.write) {
            Ok(Some(diff)) => {
                any_changes.store(true, Ordering::Relaxed);
                if !diff.is_empty() {
                    diffs.lock().unwrap().push((path.clone(), diff));
                }
            }
            Ok(None) => {}
            Err(e) => eprintln!("error processing {}: {}", path.display(), e),
        }
    });

    // Print diffs sorted by path (deterministic output)
    let mut diffs = diffs.into_inner().unwrap();
    diffs.sort_by(|a, b| a.0.cmp(&b.0));
    for (_, diff) in diffs {
        print!("{diff}");
    }

    let any_changes = any_changes.load(Ordering::Relaxed);

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
        cmd.arg(format!("+{tc}"));
    }
    cmd.arg("fmt");

    let status = cmd.status().context("failed to run cargo fmt")?;
    if !status.success() {
        bail!("cargo fmt failed with {status}");
    }
    Ok(())
}

/// Check if the git working directory is dirty (has uncommitted changes).
fn is_git_dirty(path: &Path) -> bool {
    let Ok(repo) = discover(path) else {
        return false; // Not a git repo
    };

    let Ok(platform) = repo.status(gix::progress::Discard) else {
        return false;
    };

    let Ok(iter) = platform.into_index_worktree_iter(None) else {
        return false;
    };

    iter.take(1).count() > 0
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

    bail!("Cargo.toml not found");
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

