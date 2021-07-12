mod env;

pub use crate::env::Env;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
  Num(i32),
  Bool(bool),
  Closure(String, Expr, Env<Value>),
}

impl Value {
  fn num(n: i32) -> Value {
    Value::Num(n)
  }

  fn bool(b: bool) -> Value {
    Value::Bool(b)
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

#[derive(Debug, Clone)]
pub struct Expr(Rc<ExprRaw>);

#[derive(Debug)]
enum ExprRaw {
  Var(String),
  Num(i32),
  Proc(String, Expr),
  Diff(Expr, Expr),
  IsZero(Expr),
  IfThenElse(Expr, Expr, Expr),
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

  pub fn diff(e1: Expr, e2: Expr) -> Expr {
    ExprRaw::Diff(e1, e2).wrap()
  }

  pub fn is_zero(e1: Expr) -> Expr {
    ExprRaw::IsZero(e1).wrap()
  }

  pub fn if_then_else(e1: Expr, e2: Expr, e3: Expr) -> Expr {
    ExprRaw::IfThenElse(e1, e2, e3).wrap()
  }

  pub fn let_in(x: &str, e1: Expr, e2: Expr) -> Expr {
    ExprRaw::LetIn(String::from(x), e1, e2).wrap()
  }

  pub fn proc(x: &str, e1: Expr) -> Expr {
    ExprRaw::Proc(String::from(x), e1).wrap()
  }

  pub fn apply(e1: Expr, e2: Expr) -> Expr {
    ExprRaw::Apply(e1, e2).wrap()
  }

  pub fn value_of(&self, env: &Env<Value>) -> Value {
    match &*self.0 {
      ExprRaw::Var(x) => {
        let msg = format!("not found: {}", x);
        env.lookup(x).expect(&msg)
      }
      ExprRaw::Num(n) => Value::num(*n),
      ExprRaw::Diff(e1, e2) => {
        let n1 = e1.value_of(env).to_num();
        let n2 = e2.value_of(env).to_num();
        Value::num(n1 - n2)
      }
      ExprRaw::IsZero(e1) => {
        let n1 = e1.value_of(env).to_num();
        Value::bool(0 == n1)
      }
      ExprRaw::IfThenElse(e1, e2, e3) => {
        let b1 = e1.value_of(env).to_bool();
        if b1 {
          e2.value_of(env)
        } else {
          e3.value_of(env)
        }
      }
      ExprRaw::LetIn(x, e1, e2) => {
        let v1 = e1.value_of(env);
        e2.value_of(&env.extend(&x, v1))
      }
      ExprRaw::Proc(x, e1) => Value::closure(x, e1.clone(), env.clone()),
      ExprRaw::Apply(e1, e2) => {
        let rator = e1.value_of(env);
        let rand = e2.value_of(env);
        rator.apply(rand)
      }
    }
  }
}
