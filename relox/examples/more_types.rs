use relox::lox::{Lox, LoxError};

fn main() -> Result<(), miette::Error> {
    eval("!true")?;
    eval("3 == 4")?;
    eval("3 != 4")?;
    eval("3 > 4")?;
    eval("3 < 4")?;
    eval("3 <= 4")?;
    eval("3 >= 4")?;
    eval("!(5 - 4 > 3 * 2 == !nil)")?;
    Ok(())
}

fn eval(expr: &str) -> Result<(), LoxError> {
    let mut lox = Lox::new();
    print!("{expr}: ");
    lox.eval(expr)?;
    Ok(())
}
