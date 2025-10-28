use std::fs;
use std::time::Instant;

mod common;
use common::{solve_part1, solve_part2};

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
