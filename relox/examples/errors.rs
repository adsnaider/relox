use std::io::{stdout, Write};

use relox::lox::{Lox, LoxError};

fn main() -> Result<(), LoxError<'static>> {
    env_logger::init();
    eval(
        r#"
        3 + 3
        print "hello";
        print "goodbye"+;
        what is going on
        ;
        ;
        print 3 + 3;
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
