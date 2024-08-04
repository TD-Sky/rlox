use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
pub struct Cli {
    /// Execute a script
    pub script: Option<PathBuf>,
}
