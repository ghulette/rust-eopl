use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Env<T> {
    entry: Rc<Entry<T>>,
}

#[derive(Debug)]
enum Entry<T> {
    Empty,
    Extend(Env<T>, String, T),
}

impl<T> Env<T>
where
    T: Clone,
{
    pub fn empty() -> Env<T> {
        let entry = Rc::new(Entry::Empty);
        Env { entry }
    }

    pub fn extend(&self, x: &str, v: T) -> Env<T> {
        let entry = Rc::new(Entry::Extend(self.clone(), String::from(x), v.clone()));
        Env { entry }
    }

    pub fn lookup(&self, x: &str) -> Option<T> {
        match &*self.entry {
            Entry::Empty => None,
            Entry::Extend(env, y, v) => {
                if *x == *y {
                    Some(v.clone())
                } else {
                    env.lookup(x)
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Num(i32),
    Bool(bool),
    Closure(String, Ast, Env<Value>),
}

impl Value {
    fn num(n: i32) -> Value {
        Value::Num(n)
    }

    fn bool(b: bool) -> Value {
        Value::Bool(b)
    }

    fn closure(x: &str, e: Ast, env: Env<Value>) -> Value {
        Value::Closure(String::from(x), e, env)
    }

    fn to_num(&self) -> i32 {
        match self {
            Value::Num(n) => *n,
            _ => panic!("Value::to_num"),
        }
    }

    fn to_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Value::to_bool"),
        }
    }

    fn apply(&self, v2: Value) -> Value {
        match self {
            Value::Closure(x, e, env) => e.value_of(&env.extend(x, v2)),
            _ => panic!("Value::apply"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Num(n1), Value::Num(n2)) => n1 == n2,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (_, _) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    expr: Rc<Expr>,
}

#[derive(Debug)]
enum Expr {
    Var(String),
    Num(i32),
    Proc(String, Ast),
    Diff(Ast, Ast),
    IsZero(Ast),
    IfThenElse(Ast, Ast, Ast),
    LetIn(String, Ast, Ast),
    Apply(Ast, Ast),
}

impl Ast {
    fn new(e: Expr) -> Ast {
        Ast { expr: Rc::new(e) }
    }

    pub fn var(x: &str) -> Ast {
        Ast::new(Expr::Var(String::from(x)))
    }

    pub fn num(n: i32) -> Ast {
        Ast::new(Expr::Num(n))
    }

    pub fn diff(e1: Ast, e2: Ast) -> Ast {
        Ast::new(Expr::Diff(e1, e2))
    }

    pub fn is_zero(e1: Ast) -> Ast {
        Ast::new(Expr::IsZero(e1))
    }

    pub fn if_then_else(e1: Ast, e2: Ast, e3: Ast) -> Ast {
        Ast::new(Expr::IfThenElse(e1, e2, e3))
    }

    pub fn let_in(x: &str, e1: Ast, e2: Ast) -> Ast {
        Ast::new(Expr::LetIn(String::from(x), e1, e2))
    }

    pub fn proc(x: &str, e1: Ast) -> Ast {
        Ast::new(Expr::Proc(String::from(x), e1))
    }

    pub fn apply(e1: Ast, e2: Ast) -> Ast {
        Ast::new(Expr::Apply(e1, e2))
    }

    pub fn value_of(&self, env: &Env<Value>) -> Value {
        match &*self.expr {
            Expr::Var(x) => {
                let msg = format!("not found: {}", x);
                env.lookup(x).expect(&msg)
            }
            Expr::Num(n) => Value::num(*n),
            Expr::Diff(e1, e2) => {
                let n1 = e1.value_of(env).to_num();
                let n2 = e2.value_of(env).to_num();
                Value::num(n1 - n2)
            }
            Expr::IsZero(e1) => {
                let n1 = e1.value_of(env).to_num();
                Value::bool(0 == n1)
            }
            Expr::IfThenElse(e1, e2, e3) => {
                let b1 = e1.value_of(env).to_bool();
                if b1 {
                    e2.value_of(env)
                } else {
                    e3.value_of(env)
                }
            }
            Expr::LetIn(x, e1, e2) => {
                let v1 = e1.value_of(env);
                e2.value_of(&env.extend(&x, v1))
            }
            Expr::Proc(x, e1) => Value::closure(x, e1.clone(), env.clone()),
            Expr::Apply(e1, e2) => {
                let rator = e1.value_of(env);
                let rand = e2.value_of(env);
                rator.apply(rand)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ex1() {
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
        assert_eq!(result, Value::Num(100))
    }
}
