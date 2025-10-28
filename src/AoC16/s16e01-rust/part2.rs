use std::collections::HashSet;
use std::io::{self, Read};

mod common;
use common::{parse_input, Direction};

fn solve(input: &str) -> i32 {
    let instructions = parse_input(input);

    let mut x = 0;
    let mut y = 0;
    let mut direction = Direction::North;
    let mut visited = HashSet::new();

    // Mark starting position as visited
    visited.insert((0, 0));

    for instruction in instructions {
        direction = match instruction.turn {
            'R' => direction.turn_right(),
            'L' => direction.turn_left(),
            _ => panic!("Invalid turn: {}", instruction.turn),
        };

        let (dx, dy) = direction.delta();

        // Walk step by step, checking each position
        for _ in 0..instruction.blocks {
            x += dx;
            y += dy;

            if !visited.insert((x, y)) {
                // We've been here before! Return the distance
                return x.abs() + y.abs();
            }
        }
    }

    // If we never visit a location twice, return 0 (shouldn't happen in valid puzzles)
    0
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
    fn test_sample_revisit() {
        // R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
        let input = "R8, R4, R4, R8";
        assert_eq!(solve(input), 4);
    }
}
