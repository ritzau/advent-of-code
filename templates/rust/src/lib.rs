/// Advent of Code solution

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
}

pub fn parse_input(input: &str) -> Vec<&str> {
    input.trim().lines().collect()
}

pub fn solve_part1(input: &str) -> i64 {
    let _lines = parse_input(input);
    // TODO: Implement solution
    0
}

pub fn solve_part2(input: &str) -> i64 {
    let _lines = parse_input(input);
    // TODO: Implement solution
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_1() {
        let input = "sample input";
        assert_eq!(solve_part1(input), 0);
    }

    #[test]
    fn part2_sample_1() {
        let input = "sample input";
        assert_eq!(solve_part2(input), 0);
    }
}
