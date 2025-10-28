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
