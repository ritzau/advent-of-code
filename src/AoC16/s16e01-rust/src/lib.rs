/// Common utilities for this day's solution

#[derive(Debug, Clone, Copy)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    pub fn turn_right(self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }

    pub fn turn_left(self) -> Direction {
        match self {
            Direction::North => Direction::West,
            Direction::West => Direction::South,
            Direction::South => Direction::East,
            Direction::East => Direction::North,
        }
    }

    pub fn delta(self) -> (i32, i32) {
        match self {
            Direction::North => (0, 1),
            Direction::East => (1, 0),
            Direction::South => (0, -1),
            Direction::West => (-1, 0),
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub turn: char,
    pub blocks: i32,
}

pub fn parse_input(input: &str) -> Vec<Instruction> {
    input
        .trim()
        .split(", ")
        .map(|s| {
            let turn = s.chars().next().unwrap();
            let blocks = s[1..].parse().unwrap();
            Instruction { turn, blocks }
        })
        .collect()
}

pub fn solve_part1(input: &str) -> i32 {
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

pub fn solve_part2(input: &str) -> i32 {
    use std::collections::HashSet;

    let instructions = parse_input(input);

    let mut x = 0;
    let mut y = 0;
    let mut direction = Direction::North;
    let mut visited = HashSet::new();

    visited.insert((0, 0));

    for instruction in instructions {
        direction = match instruction.turn {
            'R' => direction.turn_right(),
            'L' => direction.turn_left(),
            _ => panic!("Invalid turn: {}", instruction.turn),
        };

        let (dx, dy) = direction.delta();

        for _ in 0..instruction.blocks {
            x += dx;
            y += dy;

            if !visited.insert((x, y)) {
                return x.abs() + y.abs();
            }
        }
    }

    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_1() {
        // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
        assert_eq!(solve_part1("R2, L3"), 5);
    }

    #[test]
    fn part1_sample_2() {
        // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
        assert_eq!(solve_part1("R2, R2, R2"), 2);
    }

    #[test]
    fn part1_sample_3() {
        // R5, L5, R5, R3 leaves you 12 blocks away
        assert_eq!(solve_part1("R5, L5, R5, R3"), 12);
    }

    #[test]
    fn part2_sample_1() {
        // R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
        assert_eq!(solve_part2("R8, R4, R4, R8"), 4);
    }
}
