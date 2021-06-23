#![allow(dead_code)]

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env<T> {
    head: Rc<Link<T>>,
}

#[derive(Debug)]
enum Link<T> {
    Empty,
    Extend(Env<T>, String, T),
}

impl<T> Env<T>
where
    T: Clone,
{
    pub fn empty() -> Env<T> {
        let head = Rc::new(Link::Empty);
        Env { head }
    }

    pub fn extend(&self, x: &str, v: T) -> Env<T> {
        let link = Link::Extend(self.clone(), String::from(x), v.clone());
        let head = Rc::new(link);
        Env { head }
    }

    pub fn apply(&self, x: &str) -> Option<T> {
        match &*self.head {
            Link::Empty => None,
            Link::Extend(env, y, v) => {
                if *x == *y {
                    Some(v.clone())
                } else {
                    env.apply(x)
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Value {
    Num(i32),
    Closure(String, Rc<Expr>, Env<Value>),
}

impl Value {
    fn num(n: i32) -> Value {
        Value::Num(n)
    }

    fn closure(x: &str, e: Rc<Expr>, env: Env<Value>) -> Value {
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
enum Expr {
    Var(String),
    Num(i32),
    Proc(String, Rc<Expr>),
    Diff(Box<Expr>, Box<Expr>),
    LetIn(String, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
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

    fn proc(x: &str, e1: Expr) -> Expr {
        Expr::Proc(String::from(x), Rc::new(e1))
    }

    fn apply(e1: Expr, e2: Expr) -> Expr {
        Expr::Apply(Box::new(e1), Box::new(e2))
    }
}

fn value_of(e: &Expr, env: &Env<Value>) -> Value {
    match &*e {
        Expr::Var(x) => {
            let msg = format!("not found: {}", x);
            env.apply(x).expect(&msg)
        }
        Expr::Num(n) => Value::num(*n),
        Expr::Diff(e1, e2) => {
            let n1 = value_of(e1, env).to_num();
            let n2 = value_of(e2, env).to_num();
            Value::num(n1 - n2)
        }
        Expr::LetIn(x, e1, e2) => {
            let v1 = value_of(e1, env);
            value_of(e2, &env.extend(&x, v1))
        }
        Expr::Proc(x, e1) => Value::closure(x, e1.clone(), env.clone()),
        Expr::Apply(e1, e2) => {
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
