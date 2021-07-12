mod env;
mod expr;

use crate::env::Env;
use expr::Expr;
use expr::value_of;

fn main() {
    let env = Env::empty();
    let pgm = Expr::let_in(
        "f",
        Expr::proc("x", Expr::diff(Expr::var("x"), Expr::num(1))),
        Expr::if_then_else(
            Expr::is_zero(Expr::apply(Expr::var("f"), Expr::num(1))),
            Expr::num(100),
            Expr::num(200),
        ),
    );
    println!("{:?}", value_of(&pgm, &env));
}
