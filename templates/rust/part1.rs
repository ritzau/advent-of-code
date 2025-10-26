use std::io::{self, Read};

mod common;
use common::parse_input;

fn solve(input: &str) -> i64 {
    let _lines = parse_input(input);
    // TODO: Implement solution
    0
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let result = solve(&input);
    println!("{}", result);
}
