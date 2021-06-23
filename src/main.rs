#![allow(dead_code)]

mod env;
use env::Env;

#[derive(Debug, Clone)]
enum Expr {
    Var(String),
    Num(i32),
    Diff(Box<Expr>, Box<Expr>),
    LetIn(String, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn var(x: &str) -> Expr {
        Expr::Var(String::from(x))
    }

    fn num(n: i32) -> Expr {
        Expr::Num(n)
    }

    fn diff(e1: Expr, e2: Expr) -> Expr {
        Expr::Diff(Box::new(e1), Box::new(e2))
    }

    fn let_in(x: &str, e1: Expr, e2: Expr) -> Expr {
        Expr::LetIn(String::from(x), Box::new(e1), Box::new(e2))
    }
}

fn value_of(e: &Expr, env: &Env) -> i32 {
    match &*e {
        Expr::Var(x) => {
            let msg = format!("not found: {}", x);
            env.apply(x).expect(&msg)
        }
        Expr::Num(n) => *n,
        Expr::Diff(e1, e2) => {
            let n1 = value_of(e1, env);
            let n2 = value_of(e2, env);
            n1 - n2
        }
        Expr::LetIn(x, e1, e2) => {
            let v1 = value_of(e1, env);
            value_of(e2, &env.extend(&x, v1))
        }
    }
}

fn main() {
    let env = Env::empty();
    let pgm = Box::new(Expr::let_in(
        "x",
        Expr::num(10),
        Expr::diff(Expr::var("x"), Expr::num(1)),
    ));
    println!("{:?}", value_of(&pgm, &env));
}
