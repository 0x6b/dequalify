use std::path::{Path, PathBuf};

use anyhow::Result;
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
    target: PathBuf,

    /// Actually modify files (default: dry-run mode)
    #[arg(short, long)]
    write: bool,

    /// Print which files were modified
    #[arg(long)]
    verbose: bool,

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
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.target.is_file() {
        process_path(&cli.target, &cli)?;
    } else {
        for entry in WalkDir::new(&cli.target)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.path().extension().map(|x| x == "rs").unwrap_or(false))
        {
            process_path(entry.path(), &cli)?;
        }
    }

    Ok(())
}

fn process_path(path: &Path, cli: &Cli) -> Result<()> {
    let changed = process_file(path, &cli.ignore_roots, !cli.write, cli.alias_on_conflict)?;

    if changed && cli.verbose {
        println!("modified: {}", path.display());
    }

    Ok(())
}
