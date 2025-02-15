use std::{
    fs::File,
    io::{stdin, Read, Write},
    path::{Path, PathBuf},
    process::exit,
};

use clap::Parser;

use crate::lox::Lox;

/// Lox interpreter
#[derive(Debug, Parser)]
pub struct CliArgs {
    /// Source file to read and execute. If empty, opens a REPL instead.
    pub script: Option<PathBuf>,
}

pub struct Cli {
    lox: Lox,
}

#[derive(Debug)]
pub enum ReplError {}

#[derive(Debug)]
pub struct RunError {}

impl Cli {
    pub fn new() -> Self {
        Self { lox: Lox::new() }
    }

    pub fn run() -> ! {
        let mut app = Cli::new();
        let CliArgs { script } = CliArgs::parse();
        match script {
            Some(file) => app.run_source(&file),
            None => app.repl(),
        }
    }

    pub fn repl(&mut self) -> ! {
        let mut line = String::new();
        loop {
            print!("> ");
            std::io::stdout()
                .flush()
                .unwrap_or_else(|e| panic!("Unexpected I/O Error: {e}"));
            line.clear();
            match stdin()
                .read_line(&mut line)
                .unwrap_or_else(|e| panic!("Unexpected I/O error: {e}"))
            {
                0 => break,
                _ => {}
            }
            let _ = self.lox.eval(&line).inspect_err(|e| eprintln!("{e:?}"));
        }
        exit(0)
    }

    pub fn run_source(&mut self, source: &Path) -> ! {
        let mut content = String::new();
        let mut file = File::open(source)
            .unwrap_or_else(|e| panic!("Couldn't open the file \"{}\": {e}", source.display()));
        file.read_to_string(&mut content)
            .unwrap_or_else(|e| panic!("Couldn't read the file \"{}\": {e}", source.display()));
        self.lox
            .eval(&content)
            .unwrap_or_else(|e| eprintln!("{e:?}"));
        exit(0)
    }
}
