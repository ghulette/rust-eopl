#![allow(dead_code)]

enum Expr {
    Var(String),
    Num(i32),
    Bool(bool),
    Diff(Box<Expr>, Box<Expr>),
    IsZero(Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    LetIn(String, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn var(x: &str) -> Expr {
        Expr::Var(String::from(x))
    }

    pub fn num(n: i32) -> Expr {
        Expr::Num(n)
    }

    fn bool(b: bool) -> Expr {
        Expr::Bool(b)
    }

    pub fn diff(e1: Expr, e2: Expr) -> Expr {
        Expr::Diff(Box::new(e1), Box::new(e2))
    }

    fn is_zero(e: Expr) -> Expr {
        Expr::IsZero(Box::new(e))
    }

    fn if_then_else(e1: Expr, e2: Expr, e3: Expr) -> Expr {
        Expr::IfThenElse(Box::new(e1), Box::new(e2), Box::new(e3))
    }

    fn let_in(x: &str, e1: Expr, e2: Expr) -> Expr {
        Expr::LetIn(String::from(x), Box::new(e1), Box::new(e2))
    }
}

#[derive(Debug, Clone, Copy)]
enum Value {
    Num(i32),
    Bool(bool),
}

impl Value {
    fn to_num(&self) -> i32 {
        match self {
            Value::Num(n) => *n,
            _ => panic!("to_num"),
        }
    }

    fn to_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("to_bool"),
        }
    }
}

#[derive(Debug, Clone)]
enum Env<'a> {
    Empty,
    Extend(&'a Env<'a>, String, Value),
}

impl<'a> Env<'_> {
    fn empty() -> Env<'a> {
        Env::Empty
    }

    fn extend(&'a self, x: &str, v: Value) -> Env<'a> {
        Env::Extend(self, String::from(x), v)
    }

    fn apply(&self, x: &str) -> Option<Value> {
        match self {
            Env::Empty => None,
            Env::Extend(env0, y, v) => {
                if *x == *y {
                    Some(*v)
                } else {
                    Env::apply(env0, x)
                }
            }
        }
    }
}

fn value_of(e: &Expr, env: &Env) -> Value {
    match e {
        Expr::Var(x) => {
            let msg = format!("not found: {}", x);
            env.apply(x).expect(&msg)
        }
        Expr::Num(n) => Value::Num(*n),
        Expr::Bool(b) => Value::Bool(*b),
        Expr::Diff(e1, e2) => {
            let n1 = value_of(e1, env).to_num();
            let n2 = value_of(e2, env).to_num();
            Value::Num(n1 - n2)
        }
        Expr::IsZero(e1) => {
            let n1 = value_of(e1, env).to_num();
            Value::Bool(n1 == 0)
        }
        Expr::IfThenElse(e1, e2, e3) => {
            let c = value_of(e1, env).to_bool();
            if c {
                value_of(e2, env)
            } else {
                value_of(e3, env)
            }
        }
        Expr::LetIn(x, e1, e2) => {
            let v1 = value_of(e1, env);
            value_of(e2, &env.extend(x, v1))
        }
    }
}

fn main() {
    let env = Box::new(Env::empty());
    let pgm = Box::new(Expr::let_in("x", Expr::num(10), Expr::diff(Expr::var("x"), Expr::num(1))));
    println!("{:?}", value_of(&pgm, &env));
}
