use clap::Parser as _;
use cli::{Cli, CliArgs};

pub mod cli;
pub mod lox;

fn main() -> anyhow::Result<()> {
    let app = Cli::new();
    let CliArgs { script } = CliArgs::parse();
    match script {
        Some(file) => app.run_source(&file)?,
        None => app.repl()?,
    }
    Ok(())
}
