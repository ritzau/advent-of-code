use std::collections::HashSet;
use std::fs;
use std::time::Instant;

mod common;
use common::{parse_input, Direction};

fn solve_part1(input: &str) -> i32 {
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

fn solve_part2(input: &str) -> i32 {
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

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    // Expected answers
    const EXPECTED_PART1: i32 = 300;
    const EXPECTED_PART2: i32 = 159;

    println!("AoC 2016 Day 1: No Time for a Taxicab");
    println!("======================================");

    // Part 1
    let start = Instant::now();
    let result1 = solve_part1(&input);
    let duration1 = start.elapsed();
    let pass1 = result1 == EXPECTED_PART1;

    println!(
        "Part 1: {} {} (expected: {}) [{:.2?}]",
        if pass1 { "✅" } else { "❌" },
        result1,
        EXPECTED_PART1,
        duration1
    );

    // Part 2
    let start = Instant::now();
    let result2 = solve_part2(&input);
    let duration2 = start.elapsed();
    let pass2 = result2 == EXPECTED_PART2;

    println!(
        "Part 2: {} {} (expected: {}) [{:.2?}]",
        if pass2 { "✅" } else { "❌" },
        result2,
        EXPECTED_PART2,
        duration2
    );

    println!("Total: {:.2?}", duration1 + duration2);

    if pass1 && pass2 {
        println!("\n🌟🌟 All tests passed!");
    } else {
        println!("\n❌ Some tests failed");
        std::process::exit(1);
    }
}
