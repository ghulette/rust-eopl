use rust_eopl::{Expr, Env};

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
    let result = pgm.value_of(&env);
    println!("Result: {:?}", result);
}
