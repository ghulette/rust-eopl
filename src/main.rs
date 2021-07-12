use rust_eopl::{Ast, Env};

fn main() {
    let env = Env::empty();
    let pgm = Ast::let_in(
        "f",
        Ast::proc("x", Ast::diff(Ast::var("x"), Ast::num(1))),
        Ast::if_then_else(
            Ast::is_zero(Ast::apply(Ast::var("f"), Ast::num(1))),
            Ast::num(100),
            Ast::num(200),
        ),
    );
    let result = pgm.value_of(&env);
    println!("Result: {:?}", result);
}
