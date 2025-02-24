use std::io::{stdout, Write};

use relox::lox::{Lox, LoxError};

fn main() -> Result<(), miette::Error> {
    env_logger::init();
    eval("print \"hello\";")?;
    eval("print \"hello\" == \"hello\";")?;
    eval("print \"hello\" != \"hello\";")?;
    eval("print \"hello\" + \", world\";")?;
    eval("var x = 3;")?;
    eval("var x = \"hello\" + \" world!\";")?;
    eval("print \"hello\" + \"3\" == \"hello2\";")?;
    Ok(())
}

fn eval(expr: &str) -> Result<(), LoxError> {
    let mut lox = Lox::new();
    print!("{expr}: ");
    stdout().flush().unwrap();
    lox.eval(expr)?;
    Ok(())
}
