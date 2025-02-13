use relox::lox::{Lox, LoxError};

fn main() -> Result<(), miette::Error> {
    env_logger::init();
    eval("\"hello\"")?;
    eval("\"hello\" == \"hello\"")?;
    eval("\"hello\" != \"hello\"")?;
    eval("\"hello\" + \", world\"")?;
    eval("\"hello\" + \"3\" == \"hello2\"")?;
    eval("3 + \"hello\" ")?;
    Ok(())
}

fn eval(expr: &str) -> Result<(), LoxError> {
    let mut lox = Lox::new();
    print!("{expr}: ");
    lox.eval(expr)?;
    Ok(())
}
