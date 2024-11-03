use std::{
    fs::File,
    io::{stdin, Read, Stdout, Write},
    path::{Path, PathBuf},
};

use anyhow::Context;
use clap::Parser;
use thiserror::Error;

/// Lox interpreter
#[derive(Debug, Parser)]
struct Args {
    /// Source file to read and execute. If empty, opens a REPL instead.
    script: Option<PathBuf>,
}

pub struct Lox {}

#[derive(Error, Debug)]
pub enum ReplError {
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
}

impl Lox {
    pub fn new() -> Self {
        Self {}
    }

    pub fn exec(&self, script: &str) {
        todo!();
    }

    pub fn repl(&self) -> Result<(), ReplError> {
        let mut line = String::new();
        loop {
            print!("> ");
            std::io::stdout().flush()?;
            line.clear();
            match stdin().read_line(&mut line)? {
                0 => break,
                _ => {}
            }
            self.exec(&line);
        }
        Ok(())
    }

    pub fn run_source(&self, source: &Path) -> Result<(), std::io::Error> {
        let mut content = String::new();
        let mut file = File::open(source)?;
        file.read_to_string(&mut content)?;
        self.exec(&content);
        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    let Args { script } = Args::parse();
    let lox = Lox::new();
    match script {
        Some(file) => lox.run_source(&file)?,
        None => lox.repl()?,
    }
    Ok(())
}
