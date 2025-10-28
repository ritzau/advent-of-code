use std::io::{self, Read};

mod common;
use common::solve_part2;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let result = solve_part2(&input);
    println!("{}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_revisit() {
        // R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
        let input = "R8, R4, R4, R8";
        assert_eq!(solve_part2(input), 4);
    }
}
