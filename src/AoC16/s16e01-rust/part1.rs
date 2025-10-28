use std::io::{self, Read};

mod common;
use common::{parse_input, Direction};

fn solve(input: &str) -> i32 {
    let instructions = parse_input(input);

    let mut x = 0;
    let mut y = 0;
    let mut direction = Direction::North;

    for instruction in instructions {
        direction = match instruction.turn {
            'R' => direction.turn_right(),
            'L' => direction.turn_left(),
            _ => panic!("Invalid turn: {}", instruction.turn),
        };

        let (dx, dy) = direction.delta();
        x += dx * instruction.blocks;
        y += dy * instruction.blocks;
    }

    x.abs() + y.abs()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let result = solve(&input);
    println!("{}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_1() {
        // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
        let input = "R2, L3";
        assert_eq!(solve(input), 5);
    }

    #[test]
    fn test_sample_2() {
        // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
        let input = "R2, R2, R2";
        assert_eq!(solve(input), 2);
    }

    #[test]
    fn test_sample_3() {
        // R5, L5, R5, R3 leaves you 12 blocks away
        let input = "R5, L5, R5, R3";
        assert_eq!(solve(input), 12);
    }
}
