use s16e01_rust::{solve_part1, solve_part2};
use std::io::{self, Read};
use std::time::Instant;

fn main() {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("Failed to read from stdin");

    println!("AoC 2016 Day 1: No Time for a Taxicab");
    println!("======================================");

    // Part 1
    let start = Instant::now();
    let result1 = solve_part1(&input);
    let duration1 = start.elapsed();
    const EXPECTED_PART1: i32 = 300;
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
    const EXPECTED_PART2: i32 = 159;
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
