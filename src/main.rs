#![allow(dead_code)]
mod env;
use crate::env::Env;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Value {
    Num(i32),
    Closure(String, Expr, Env<Value>),
}

impl Value {
    fn num(n: i32) -> Value {
        Value::Num(n)
    }

    fn closure(x: &str, e: Expr, env: Env<Value>) -> Value {
        Value::Closure(String::from(x), e, env)
    }

    fn to_num(&self) -> i32 {
        match self {
            Value::Num(n) => *n,
            _ => panic!("Value::to_num"),
        }
    }

    fn apply(&self, v2: Value) -> Value {
        match self {
            Value::Closure(x, e, env) => value_of(e, &env.extend(x, v2)),
            _ => panic!("Value::apply"),
        }
    }
}


#[derive(Debug, Clone)]
pub struct Expr(Rc<ExprRaw>);

#[derive(Debug)]
enum ExprRaw {
    Var(String),
    Num(i32),
    Proc(String, Expr),
    Diff(Expr, Expr),
    LetIn(String, Expr, Expr),
    Apply(Expr, Expr),
}

impl ExprRaw {
    fn wrap(self) -> Expr {
        Expr(Rc::new(self))
    }
}

impl Expr {
    pub fn var(x: &str) -> Expr {
        ExprRaw::Var(String::from(x)).wrap()
    }

    pub fn num(n: i32) -> Expr {
        ExprRaw::Num(n).wrap()
    }

    fn diff(e1: Expr, e2: Expr) -> Expr {
        ExprRaw::Diff(e1, e2).wrap()
    }

    fn let_in(x: &str, e1: Expr, e2: Expr) -> Expr {
        ExprRaw::LetIn(String::from(x), e1, e2).wrap()
    }

    fn proc(x: &str, e1: Expr) -> Expr {
        ExprRaw::Proc(String::from(x), e1).wrap()
    }

    fn apply(e1: Expr, e2: Expr) -> Expr {
        ExprRaw::Apply(e1, e2).wrap()
    }
}

fn value_of(e: &Expr, env: &Env<Value>) -> Value {
    match &*e.0 {
        ExprRaw::Var(x) => {
            let msg = format!("not found: {}", x);
            env.lookup(x).expect(&msg)
        }
        ExprRaw::Num(n) => Value::num(*n),
        ExprRaw::Diff(e1, e2) => {
            let n1 = value_of(e1, env).to_num();
            let n2 = value_of(e2, env).to_num();
            Value::num(n1 - n2)
        }
        ExprRaw::LetIn(x, e1, e2) => {
            let v1 = value_of(e1, env);
            value_of(e2, &env.extend(&x, v1))
        }
        ExprRaw::Proc(x, e1) => Value::closure(x, e1.clone(), env.clone()),
        ExprRaw::Apply(e1, e2) => {
            let rator = value_of(e1, env);
            let rand = value_of(e2, env);
            rator.apply(rand)
        }
    }
}

fn main() {
    let env = Env::empty();
    let pgm = Expr::let_in(
        "f",
        Expr::proc("x", Expr::diff(Expr::var("x"), Expr::num(1))),
        Expr::apply(Expr::var("f"), Expr::num(10)),
    );
    println!("{:?}", value_of(&pgm, &env));
}
