use std::io::{stdout, Write};

use relox::lox::{Lox, LoxError};

fn main() -> Result<(), miette::Error> {
    env_logger::init();
    eval(
        r#"
            var a = 3;
            var y = 4;
            var z;
            print a;
            print b;
            print z;
        "#,
    )?;

    Ok(())
}

fn eval(expr: &str) -> Result<(), LoxError> {
    let mut lox = Lox::new();
    print!("{expr}: ");
    stdout().flush().unwrap();
    lox.eval(expr)?;
    Ok(())
}
