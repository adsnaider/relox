use relox::lox::{
    ast::{BinaryExpr, Expr, ExprPrinter, Grouping, Literal, UnaryExpr},
    lexer::{Lexeme, Token, TokenValue},
};

fn main() {
    let expr = Expr::Binary(Box::new(BinaryExpr {
        lhs: Expr::Unary(Box::new(UnaryExpr {
            op: Token {
                value: TokenValue::Minus,
                lexeme: Lexeme::new("-", 0),
            },
            rhs: Expr::Literal(Box::new(Literal {
                value: Token {
                    value: TokenValue::Number(123.0),
                    lexeme: Lexeme::new("123", 0),
                },
            })),
        })),
        rhs: Expr::Grouping(Box::new(Grouping {
            expr: Expr::Literal(Box::new(Literal {
                value: Token {
                    value: TokenValue::Number(45.67),
                    lexeme: Lexeme::new("45.67", 0),
                },
            })),
        })),
        op: Token {
            value: TokenValue::Star,
            lexeme: Lexeme::new("*", 0),
        },
    }));
    let mut printer = ExprPrinter;
    printer.print(&expr);
}
