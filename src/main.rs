use rust_eopl::{Env, parser};
use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer).expect("failed to read stdin");
    let env = Env::empty();
    let (_, pgm) = parser::parse(&buffer).expect("failed to parse program");
    let result = pgm.value_of(&env);
    println!("Result: {:?}", result);
}
