use std::io::{self, Read};
use aoc_solution::solve_part1;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let result = solve_part1(&input);
    println!("{}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_1() {
        // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
        let input = "R2, L3";
        assert_eq!(solve_part1(input), 5);
    }

    #[test]
    fn test_sample_2() {
        // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
        let input = "R2, R2, R2";
        assert_eq!(solve_part1(input), 2);
    }

    #[test]
    fn test_sample_3() {
        // R5, L5, R5, R3 leaves you 12 blocks away
        let input = "R5, L5, R5, R3";
        assert_eq!(solve_part1(input), 12);
    }
}
