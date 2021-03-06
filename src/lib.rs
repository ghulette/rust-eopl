use std::convert::TryFrom;

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

impl TryFrom<&Value> for i32 {
    type Error = &'static str;

    fn try_from(val: &Value) -> Result<Self, Self::Error> {
        match val {
            Value::Num(n) => Ok(*n),
            _ => Err("Value is not a number"),
        }
    }
}

impl TryFrom<&Value> for bool {
    type Error = &'static str;

    fn try_from(val: &Value) -> Result<Self, Self::Error> {
        match val {
            Value::Bool(b) => Ok(*b),
            _ => Err("Value is not a bool"),
        }
    }
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
        i32::try_from(self).expect("Value::to_num")
    }

    fn to_bool(&self) -> bool {
        bool::try_from(self).expect("Value::to_bool")
    }

    fn apply(&self, v2: &Self) -> Self {
        match self {
            Value::Closure(x, e, env) => e.eval(&env.extend(x, v2)),
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Num(i32),
    Proc(String, Box<Expr>),
    Binop(Op, Box<Expr>, Box<Expr>),
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

    pub fn binop(op: Op, e1: Self, e2: Self) -> Self {
        Self::Binop(op, Box::new(e1), Box::new(e2))
    }

    pub fn add(e1: Self, e2: Self) -> Self {
        Self::binop(Op::Add, e1, e2)
    }

    pub fn sub(e1: Self, e2: Self) -> Self {
        Self::binop(Op::Sub, e1, e2)
    }

    pub fn mul(e1: Self, e2: Self) -> Self {
        Self::binop(Op::Mul, e1, e2)
    }

    pub fn div(e1: Self, e2: Self) -> Self {
        Self::binop(Op::Div, e1, e2)
    }

    pub fn eq(e1: Self, e2: Self) -> Self {
        Self::binop(Op::Eq, e1, e2)
    }

    pub fn lt(e1: Self, e2: Self) -> Self {
        Self::binop(Op::Lt, e1, e2)
    }

    pub fn and(e1: Self, e2: Self) -> Self {
        Self::binop(Op::And, e1, e2)
    }

    pub fn or(e1: Self, e2: Self) -> Self {
        Self::binop(Op::Or, e1, e2)
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

    pub fn eval(&self, env: &Env) -> Value {
        use Expr::*;
        match self {
            Var(x) => {
                let msg = format!("not found: {}", x);
                env.lookup(x).expect(&msg)
            }
            Num(n) => Value::num(*n),
            Binop(op, e1, e2) => match op {
                Op::Add | Op::Sub | Op::Mul | Op::Div => {
                    let n1 = e1.eval(env).to_num();
                    let n2 = e2.eval(env).to_num();
                    let r = match op {
                        Op::Add => n1 + n2,
                        Op::Sub => n1 - n2,
                        Op::Mul => n1 * n2,
                        Op::Div => n1 / n2,
                        _ => panic!("{:?} is not a valid arithmetic operator", op),
                    };
                    Value::num(r)
                }
                Op::And | Op::Or => {
                    let b1 = e1.eval(env).to_bool();
                    let b2 = e2.eval(env).to_bool();
                    let r = match op {
                        Op::And => b1 && b2,
                        Op::Or => b1 || b2,
                        _ => panic!("{:?} is not a valid boolean operator", op),
                    };
                    Value::bool(r)
                }
                Op::Lt => {
                    let n1 = e1.eval(env).to_num();
                    let n2 = e2.eval(env).to_num();
                    Value::bool(n1 < n2)
                }
                Op::Eq => {
                    let v1 = e1.eval(env);
                    let v2 = e2.eval(env);
                    Value::bool(v1 == v2)
                }
            },
            IfThenElse(e1, e2, e3) => {
                let b1 = e1.eval(env).to_bool();
                if b1 {
                    e2.eval(env)
                } else {
                    e3.eval(env)
                }
            }
            LetIn(x, e1, e2) => {
                let v1 = e1.eval(env);
                e2.eval(&env.extend(&x, &v1))
            }
            LetRec(proc, x, e1, e2) => e2.eval(&env.extend_rec(&proc, &x, e1)),
            Proc(x, e1) => Value::closure(x, e1, env),
            Apply(e1, e2) => {
                let rator = e1.eval(env);
                let rand = e2.eval(env);
                rator.apply(&rand)
            }
        }
    }
}

pub mod parser {
    use super::{Expr, Op};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::{alpha1, alphanumeric1, char, multispace0, one_of};
    use nom::combinator::{eof, map, map_res, opt, recognize, value, verify};
    use nom::multi::{many0, many1};
    use nom::sequence::{delimited, pair, terminated};
    use nom::IResult;
    use phf::phf_set;
    use std::str::FromStr;

    static KEYWORDS: phf::Set<&'static str> = phf_set! {
        "let",
        "letrec",
        "in",
        "proc",
        "if",
        "then",
        "else",
    };

    fn is_keyword(kw: &str) -> bool {
        KEYWORDS.contains(kw)
    }

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
    fn literal<'a>(s: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
        lexeme(tag(s))
    }

    fn ident(input: &str) -> IResult<&str, &str> {
        verify(
            lexeme(recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            ))),
            |s| !is_keyword(s),
        )(input)
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

    fn let_rec(input: &str) -> IResult<&str, Expr> {
        let (input, _) = literal("letrec")(input)?;
        let (input, f) = ident(input)?;
        let (input, x) = ident(input)?;
        let (input, _) = literal("=")(input)?;
        let (input, e1) = expr(input)?;
        let (input, _) = literal("in")(input)?;
        let (input, e2) = expr(input)?;
        Ok((input, Expr::let_rec(f, x, e1, e2)))
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
        alt((let_rec, let_in, if_then_else, proc, rel_expr))(input)
    }

    fn rel_op(input: &str) -> IResult<&str, Op> {
        alt((value(Op::Lt, literal("<")), value(Op::Eq, literal("="))))(input)
    }

    fn rel_expr(input: &str) -> IResult<&str, Expr> {
        let (input, e0) = arith_expr(input)?;
        let (input, es) = many0(pair(rel_op, arith_expr))(input)?;
        let e = es
            .into_iter()
            .fold(e0, |acc, (op, expr)| Expr::binop(op, acc, expr));
        Ok((input, e))
    }

    fn arith_op(input: &str) -> IResult<&str, Op> {
        alt((value(Op::Add, literal("+")), value(Op::Sub, literal("-"))))(input)
    }

    fn arith_expr(input: &str) -> IResult<&str, Expr> {
        let (input, e0) = term_expr(input)?;
        let (input, es) = many0(pair(arith_op, term_expr))(input)?;
        let e = es
            .into_iter()
            .fold(e0, |acc, (op, expr)| Expr::binop(op, acc, expr));
        Ok((input, e))
    }

    fn term_op(input: &str) -> IResult<&str, Op> {
        alt((value(Op::Mul, literal("*")), value(Op::Div, literal("/"))))(input)
    }

    fn term_expr(input: &str) -> IResult<&str, Expr> {
        let (input, e0) = app_expr(input)?;
        let (input, es) = many0(pair(term_op, app_expr))(input)?;
        let e = es
            .into_iter()
            .fold(e0, |acc, (op, expr)| Expr::binop(op, acc, expr));
        Ok((input, e))
    }

    fn app_expr(input: &str) -> IResult<&str, Expr> {
        let (input, e0) = basic_expr(input)?;
        let (input, es) = many0(basic_expr)(input)?;
        let e = es.into_iter().fold(e0, |acc, expr| Expr::apply(acc, expr));
        Ok((input, e))
    }

    fn basic_expr(input: &str) -> IResult<&str, Expr> {
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

    struct Example {
        program: &'static str,
        expected_result: Value,
    }

    fn run_example(ex: Example) {
        let (_, pgm) = parser::parse(&ex.program).expect("parse failed");
        let env = Env::empty();
        assert_eq!(pgm.eval(&env), ex.expected_result)
    }

    const EX1: Example = Example {
        expected_result: Value::Num(11),
        program: r#"
let x = 5 in
let y = 6 in
x + y
"#,
    };

    const EX2: Example = Example {
        expected_result: Value::Num(3),
        program: r#"
10 - 3 - 4
"#,
    };

    const EX3: Example = Example {
        expected_result: Value::Num(9),
        program: r#"
1 + 2 * 3 + 4 / 2
"#,
    };

    const EX4: Example = Example {
        expected_result: Value::Num(4),
        program: r#"
let add = proc x -> proc y -> x + y in
add 10 -6
"#,
    };

    const FACT: Example = Example {
        expected_result: Value::Num(120),
        program: r#"
letrec fact n =
  if n = 0 then 1 else n * fact (n - 1)
in
  fact 5
"#,
    };

    const DOUBLE: Example = Example {
        expected_result: Value::Num(200),
        program: r#"
letrec double x =
  if x = 0 then 0
  else double (x - 1) + 2
in
  double 100
"#,
    };

    #[test]
    fn examples() {
        run_example(EX1);
        run_example(EX2);
        run_example(EX3);
        run_example(EX4);
        run_example(FACT);
        run_example(DOUBLE);
    }

    #[test]
    fn parse_negative_number() {
        match parser::parse("  -00324 ") {
            Ok((_, e)) => assert_eq!(e, Expr::num(-324)),
            Err(err) => panic!("{}", err.to_string()),
        }
    }
}
