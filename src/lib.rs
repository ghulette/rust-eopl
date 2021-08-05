#[derive(Debug, Clone)]
pub enum Env {
    Empty,
    Extend(Box<Env>, String, Value),
    ExtendRec(Box<Env>, String, String, Box<Expr>),
}

impl Env {
    pub fn empty() -> Self {
        Env::Empty
    }

    pub fn extend(&self, x: &str, v: &Value) -> Self {
        Env::Extend(Box::new(self.clone()), String::from(x), v.clone())
    }

    pub fn extend_rec(&self, proc_id: &str, var_id: &str, body: &Expr) -> Self {
        Env::ExtendRec(
            Box::new(self.clone()),
            String::from(proc_id),
            String::from(var_id),
            Box::new(body.clone()),
        )
    }

    pub fn lookup(&self, x: &str) -> Option<Value> {
        match self {
            Env::Empty => None,
            Env::Extend(env, var_id, val) => {
                if *x == *var_id {
                    Some(val.clone())
                } else {
                    env.lookup(x)
                }
            }
            Env::ExtendRec(env, proc_id, var_id, body) => {
                if *x == *proc_id {
                    Some(Value::closure(var_id, body, self))
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
    Closure(String, Box<Expr>, Box<Env>),
}

impl Value {
    fn num(n: i32) -> Self {
        Value::Num(n)
    }

    fn bool(b: bool) -> Self {
        Value::Bool(b)
    }

    fn closure(x: &str, body: &Expr, env: &Env) -> Self {
        Value::Closure(
            String::from(x),
            Box::new(body.clone()),
            Box::new(env.clone()),
        )
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

    fn apply(&self, v2: &Self) -> Self {
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Num(i32),
    Proc(String, Box<Expr>),
    Diff(Box<Expr>, Box<Expr>),
    IsZero(Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    LetIn(String, Box<Expr>, Box<Expr>),
    LetRec(String, String, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn var(x: &str) -> Self {
        Self::Var(String::from(x))
    }

    pub fn num(n: i32) -> Self {
        Self::Num(n)
    }

    pub fn diff(e1: Self, e2: Self) -> Self {
        Self::Diff(Box::new(e1), Box::new(e2))
    }

    pub fn is_zero(e1: Self) -> Self {
        Self::IsZero(Box::new(e1))
    }

    pub fn if_then_else(e1: Self, e2: Self, e3: Self) -> Self {
        Self::IfThenElse(Box::new(e1), Box::new(e2), Box::new(e3))
    }

    pub fn let_in(x: &str, e1: Self, e2: Self) -> Self {
        Self::LetIn(String::from(x), Box::new(e1), Box::new(e2))
    }

    pub fn let_rec(proc: &str, x: &str, e1: Self, e2: Self) -> Self {
        Self::LetRec(
            String::from(proc),
            String::from(x),
            Box::new(e1),
            Box::new(e2),
        )
    }

    pub fn proc(x: &str, e1: Self) -> Self {
        Self::Proc(String::from(x), Box::new(e1))
    }

    pub fn apply(e1: Self, e2: Self) -> Self {
        Self::Apply(Box::new(e1), Box::new(e2))
    }

    pub fn value_of(&self, env: &Env) -> Value {
        use Expr::*;
        match self {
            Var(x) => {
                let msg = format!("not found: {}", x);
                env.lookup(x).expect(&msg)
            }
            Num(n) => Value::num(*n),
            Diff(e1, e2) => {
                let n1 = e1.value_of(env).to_num();
                let n2 = e2.value_of(env).to_num();
                Value::num(n1 - n2)
            }
            IsZero(e1) => {
                let n1 = e1.value_of(env).to_num();
                Value::bool(0 == n1)
            }
            IfThenElse(e1, e2, e3) => {
                let b1 = e1.value_of(env).to_bool();
                if b1 {
                    e2.value_of(env)
                } else {
                    e3.value_of(env)
                }
            }
            LetIn(x, e1, e2) => {
                let v1 = e1.value_of(env);
                e2.value_of(&env.extend(&x, &v1))
            }
            LetRec(proc, x, e1, e2) => e2.value_of(&env.extend_rec(&proc, &x, e1)),
            Proc(x, e1) => Value::closure(x, e1, env),
            Apply(e1, e2) => {
                let rator = e1.value_of(env);
                let rand = e2.value_of(env);
                rator.apply(&rand)
            }
        }
    }
}

pub mod parser {
    use super::Expr;
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1, char, multispace0, one_of};
    use nom::combinator::{eof, map, map_res, opt, recognize, value};
    use nom::multi::{many0, many1};
    use nom::sequence::{delimited, pair, terminated};
    use nom::IResult;
    use std::str::FromStr;

    fn lexeme<'a, O>(
        inner: impl FnMut(&'a str) -> IResult<&'a str, O>,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
        terminated(inner, multispace0)
    }

    fn all_of<'a, O>(
        inner: impl FnMut(&'a str) -> IResult<&'a str, O>,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
        delimited(multispace0, inner, eof)
    }

    fn parens<'a, O>(
        inner: impl FnMut(&'a str) -> IResult<&'a str, O>,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
        delimited(lexeme(char('(')), inner, lexeme(char(')')))
    }

    #[allow(unused)]
    fn literal<'a>(s: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, ()> {
        value((), lexeme(tag(s)))
    }

    fn ident(input: &str) -> IResult<&str, &str> {
        lexeme(recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )))(input)
    }

    fn num(input: &str) -> IResult<&str, i32> {
        map_res(
            lexeme(recognize(pair(opt(char('-')), many1(one_of("0123456789"))))),
            |s| i32::from_str(s),
        )(input)
    }

    fn let_in(input: &str) -> IResult<&str, Expr> {
        let (input, _) = literal("let")(input)?;
        let (input, x) = ident(input)?;
        let (input, _) = literal("=")(input)?;
        let (input, e1) = expr(input)?;
        let (input, _) = literal("in")(input)?;
        let (input, e2) = expr(input)?;
        Ok((input, Expr::let_in(x, e1, e2)))
    }

    fn if_then_else(input: &str) -> IResult<&str, Expr> {
        let (input, _) = literal("if")(input)?;
        let (input, e1) = expr(input)?;
        let (input, _) = literal("then")(input)?;
        let (input, e2) = expr(input)?;
        let (input, _) = literal("else")(input)?;
        let (input, e3) = expr(input)?;
        Ok((input, Expr::if_then_else(e1, e2, e3)))
    }

    fn proc(input: &str) -> IResult<&str, Expr> {
        let (input, _) = literal("proc")(input)?;
        let (input, f) = ident(input)?;
        let (input, _) = literal("->")(input)?;
        let (input, e) = expr(input)?;
        Ok((input, Expr::proc(f, e)))
    }

    fn expr(input: &str) -> IResult<&str, Expr> {
        alt((let_in, if_then_else, proc, expr0))(input)
    }

    fn expr0(input: &str) -> IResult<&str, Expr> {
        atomic(input)
    }

    fn atomic(input: &str) -> IResult<&str, Expr> {
        alt((map(ident, Expr::var), map(num, Expr::num), parens(expr)))(input)
    }

    pub fn parse(input: &str) -> IResult<&str, Expr> {
        let (input, e) = all_of(expr)(input)?;
        Ok((input, e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ex1() {
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
        assert_eq!(result, Value::Num(100))
    }

    #[test]
    fn ex2() {
        let env = Env::empty();
        let pgm = Expr::let_rec(
            "double",
            "x",
            Expr::if_then_else(
                Expr::is_zero(Expr::var("x")),
                Expr::num(0),
                Expr::diff(
                    Expr::apply(
                        Expr::var("double"),
                        Expr::diff(Expr::var("x"), Expr::num(1)),
                    ),
                    Expr::num(-2),
                ),
            ),
            Expr::apply(Expr::var("double"), Expr::num(6)),
        );
        let result = pgm.value_of(&env);
        assert_eq!(result, Value::Num(12))
    }

    #[test]
    fn parser_test1() {
        match parser::parse("foo") {
            Ok((_, e)) => assert_eq!(e, Expr::var("foo")),
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    fn parser_test2() {
        match parser::parse("123") {
            Ok((_, e)) => assert_eq!(e, Expr::num(123)),
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    fn parser_test3() {
        match parser::parse("  (  ( -00324 ) ) ") {
            Ok((_, e)) => assert_eq!(e, Expr::num(-324)),
            Err(err) => panic!("{}", err.to_string()),
        }
    }

    #[test]
    fn parser_test4() {
        match parser::parse(" let foo = -5 in \n let bar = (2) in \n x  ") {
            Ok((_, e)) => assert_eq!(
                e,
                Expr::let_in(
                    "foo",
                    Expr::num(-5),
                    Expr::let_in("bar", Expr::num(2), Expr::var("x"))
                )
            ),
            Err(err) => panic!("{}", err.to_string()),
        }
    }
}
