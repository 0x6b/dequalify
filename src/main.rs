mod rewrite;

use std::{
    env::args,
    ffi::OsStr,
    fs::read_to_string,
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{Context, Result, anyhow, bail};
use clap::Parser;
use dunce::canonicalize;
use gix::discover;
use ignore::WalkBuilder;
use rayon::prelude::*;
use rewrite::process_file;
use serde::Deserialize;
use toml::from_str;

#[derive(Debug, Parser)]
#[command(author, version, about)]
/// Rewrite fully-qualified function calls into imported short names.
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
    let mut raw_args: Vec<String> = args().collect();
    if raw_args.get(1).is_some_and(|s| s == "dequalify") {
        raw_args.remove(1);
    }
    let cli = Cli::parse_from(&raw_args);

    let root = canonicalize(&cli.target)
        .with_context(|| format!("canonicalize {}", cli.target.display()))?;
    let cargo_toml = find_cargo_toml(&root)?;
    let (virtual_root, members) = load_workspace(&cargo_toml)?;

    if cli.write && !cli.allow_dirty && is_git_dirty(&root) {
        bail!("uncommitted changes; commit/stash or use --allow-dirty");
    }

    let crate_roots = workspace_crate_roots(&cargo_toml, virtual_root, &members);
    let rs_files: Vec<PathBuf> = crate_roots
        .iter()
        .flat_map(|r| {
            WalkBuilder::new(r)
                .standard_filters(true)
                .hidden(false)
                .follow_links(false)
                .build()
                .filter_map(Result::ok)
                .filter(|e| e.path().is_file() && e.path().extension() == Some(OsStr::new("rs")))
                .map(|e| e.into_path())
        })
        .collect();

    let results: Vec<_> = rs_files
        .par_iter()
        .map(|p| (p.clone(), process_file(p, &cli.ignore_roots, !cli.write)))
        .collect();

    let mut diffs: Vec<_> = results
        .iter()
        .filter_map(|(p, r)| match r {
            Ok(Some(d)) if !d.is_empty() => Some((p.clone(), d.clone())),
            Err(e) => {
                eprintln!("error {}: {e}", p.display());
                None
            }
            _ => None,
        })
        .collect();
    diffs.sort_by(|a, b| a.0.cmp(&b.0));
    for (_, d) in &diffs {
        print!("{d}");
    }

    let any_changes = results.iter().any(|(_, r)| matches!(r, Ok(Some(_))));
    if any_changes && !cli.write {
        eprintln!("# Run with `-w` to apply, or `-w -f` to also format.");
    }
    if any_changes
        && cli.write
        && let Some(tc) = &cli.fmt
    {
        run_cargo_fmt(tc.as_deref())?;
    }
    Ok(())
}

fn run_cargo_fmt(tc: Option<&str>) -> Result<()> {
    let mut cmd = Command::new("cargo");
    if let Some(t) = tc {
        cmd.arg(format!("+{t}"));
    }
    let status = cmd.arg("fmt").status().context("cargo fmt")?;
    if !status.success() {
        bail!("cargo fmt failed");
    }
    Ok(())
}

fn is_git_dirty(path: &Path) -> bool {
    let Ok(repo) = discover(path) else { return false };
    let Ok(platform) = repo.status(gix::progress::Discard) else { return false };
    let Ok(iter) = platform.into_index_worktree_iter(None) else { return false };
    iter.take(1).count() > 0
}

#[derive(Deserialize)]
struct CargoToml {
    workspace: Option<Workspace>,
    package: Option<Package>,
}
#[derive(Deserialize)]
struct Workspace {
    members: Option<Vec<String>>,
}
#[derive(Deserialize)]
struct Package {}

fn find_cargo_toml(start: &Path) -> Result<PathBuf> {
    start
        .ancestors()
        .map(|d| d.join("Cargo.toml"))
        .find(|c| c.is_file())
        .ok_or_else(|| anyhow!("Cargo.toml not found"))
}

fn load_workspace(path: &Path) -> Result<(bool, Vec<String>)> {
    let parsed: CargoToml = from_str(&read_to_string(path)?)?;
    let mut members = parsed.workspace.and_then(|w| w.members).unwrap_or_default();
    members.sort();
    members.dedup();
    Ok((parsed.package.is_none(), members))
}

fn workspace_crate_roots(
    cargo_toml: &Path,
    virtual_root: bool,
    members: &[String],
) -> Vec<PathBuf> {
    let root = cargo_toml.parent().unwrap_or(Path::new("."));
    let mut roots: Vec<PathBuf> = members.iter().map(|m| root.join(m)).collect();
    if !virtual_root || roots.is_empty() {
        roots.push(root.to_path_buf());
    }
    roots.sort();
    roots.dedup();
    roots
}
