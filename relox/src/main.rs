use cli::Cli;

pub mod cli;
pub mod lox;

fn main() -> anyhow::Result<()> {
    Cli::run()
}
