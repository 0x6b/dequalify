use std::{
    path::{Path, PathBuf},
    process::Command,
};

use anyhow::{Context, Result};
use clap::Parser;
use dequalify::process_file;
use walkdir::WalkDir;

/// Rewrite fully-qualified function calls into imported short names.
///
/// Example:
///   tokio::task::spawn(foo())  ==>  use tokio::task::spawn; spawn(foo())
///
/// By default:
///   - Rewrites only when there is no name conflict
///   - If the short name already exists (import/local), it skips and prints a warning
///
/// With --alias-on-conflict:
///   - On conflict, generates an alias: use tokio::task::spawn as tokio_task_spawn;
///     tokio_task_spawn(foo());
#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// File or directory to process (recursively for directories)
    #[arg(default_value = ".")]
    target: PathBuf,

    /// Actually modify files (default: dry-run mode)
    #[arg(short, long)]
    write: bool,

    /// Comma-separated list of top-level roots to ignore (e.g. "std,core,alloc")
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

    /// Run cargo fmt after writing changes. Optionally specify a toolchain (e.g., --fmt=nightly)
    #[arg(short, long, value_name = "TOOLCHAIN")]
    fmt: Option<Option<String>>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut any_changes = false;

    if cli.target.is_file() {
        any_changes |= process_path(&cli.target, &cli)?;
    } else {
        for entry in WalkDir::new(&cli.target)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.path().extension().map(|x| x == "rs").unwrap_or(false))
        {
            any_changes |= process_path(entry.path(), &cli)?;
        }
    }

    if any_changes && !cli.write {
        eprintln!("# Run with `--write` to apply changes, or `--write --fmt` to also format.");
    }

    if any_changes && cli.write {
        if let Some(toolchain) = &cli.fmt {
            run_cargo_fmt(toolchain.as_deref())?;
        }
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

fn process_path(path: &Path, cli: &Cli) -> Result<bool> {
    let changed = process_file(path, &cli.ignore_roots, !cli.write, cli.alias_on_conflict)?;
    Ok(changed)
}
