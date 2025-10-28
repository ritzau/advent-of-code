use s16e01::solve_part1;
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let result = solve_part1(&input);
    println!("{}", result);
}
