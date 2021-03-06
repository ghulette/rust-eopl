use rust_eopl::{Env, parser};
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer).expect("failed to read stdin");
    let (_, pgm) = parser::parse(&buffer).expect("failed to parse program");
    let env = Env::empty();
    let result = pgm.eval(&env);
    println!("Result: {:?}", result);
}
