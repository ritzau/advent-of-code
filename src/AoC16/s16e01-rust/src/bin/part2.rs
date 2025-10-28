use std::io::{self, Read};
use s16e01::solve_part2;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let result = solve_part2(&input);
    println!("{}", result);
}
