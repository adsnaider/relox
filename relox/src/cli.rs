use std::{
    fs::File,
    io::{stdin, Read, Write},
    path::{Path, PathBuf},
};

use clap::Parser;
use thiserror::Error;

use crate::lox::{Lox, LoxError};

/// Lox interpreter
#[derive(Debug, Parser)]
pub struct CliArgs {
    /// Source file to read and execute. If empty, opens a REPL instead.
    pub script: Option<PathBuf>,
}

pub struct Cli {
    lox: Lox,
}

#[derive(Error, Debug)]
pub enum ReplError {
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
}

#[derive(Error, Debug)]
pub enum RunError {
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    LoxError(#[from] LoxError),
}

impl Cli {
    pub fn new() -> Self {
        Self { lox: Lox::new() }
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
            if let Err(e) = self.lox.eval(&line) {
                println!("{}", e);
            }
        }
        println!("");
        Ok(())
    }

    pub fn run_source(&self, source: &Path) -> Result<(), RunError> {
        let mut content = String::new();
        let mut file = File::open(source)?;
        file.read_to_string(&mut content)?;
        self.lox.eval(&content)?;
        Ok(())
    }
}
