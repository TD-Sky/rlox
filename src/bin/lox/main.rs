use std::fs;
use std::process::exit;

use anyhow::Context;
use clap::Parser;
use rlox::cli::Cli;
use rlox::{rep, run};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    if let Some(script) = &cli.script {
        let code =
            fs::read_to_string(script).with_context(|| format!("script `{}`", script.display()))?;
        run(script, &code);
    } else {
        repl()?;
    }

    Ok(())
}

fn repl() -> rustyline::Result<()> {
    let mut rl = DefaultEditor::new()?;
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line)?;
                rep(&line);
            }
            Err(ReadlineError::Eof) => return Ok(()),
            Err(ReadlineError::Interrupted) => {
                eprintln!("user exit");
                exit(1);
            }
            Err(e) => return Err(e),
        }
    }
}
