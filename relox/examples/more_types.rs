use relox::lox::Lox;

fn main() -> Result<(), miette::Error> {
    let mut lox = Lox::new();
    lox.eval("!true")?;

    Ok(())
}
