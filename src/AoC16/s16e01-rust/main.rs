use std::fs;
use std::time::Instant;

mod common;
use common::{solve_part1, solve_part2};

fn main() {
    println!("AoC 2016 Day 1: No Time for a Taxicab");
    println!("======================================\n");

    let mut all_passed = true;

    // Sample tests for Part 1
    println!("Part 1 Sample Tests:");
    let samples_part1 = [
        ("R2, L3", 5, "2 blocks East and 3 blocks North"),
        ("R2, R2, R2", 2, "2 blocks due South"),
        ("R5, L5, R5, R3", 12, "12 blocks away"),
    ];

    for (input, expected, description) in samples_part1 {
        let result = solve_part1(input);
        let pass = result == expected;
        all_passed &= pass;
        println!(
            "  {} {} (expected: {}) - {}",
            if pass { "✅" } else { "❌" },
            result,
            expected,
            description
        );
    }

    // Sample test for Part 2
    println!("\nPart 2 Sample Tests:");
    let result = solve_part2("R8, R4, R4, R8");
    let pass = result == 4;
    all_passed &= pass;
    println!(
        "  {} {} (expected: 4) - first location visited twice",
        if pass { "✅" } else { "❌" },
        result
    );

    // Actual input tests
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    println!("\nActual Input:");

    // Part 1
    let start = Instant::now();
    let result1 = solve_part1(&input);
    let duration1 = start.elapsed();
    const EXPECTED_PART1: i32 = 300;
    let pass1 = result1 == EXPECTED_PART1;
    all_passed &= pass1;

    println!(
        "  Part 1: {} {} (expected: {}) [{:.2?}]",
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
    all_passed &= pass2;

    println!(
        "  Part 2: {} {} (expected: {}) [{:.2?}]",
        if pass2 { "✅" } else { "❌" },
        result2,
        EXPECTED_PART2,
        duration2
    );

    println!("  Total: {:.2?}", duration1 + duration2);

    if all_passed {
        println!("\n🌟🌟 All tests passed!");
    } else {
        println!("\n❌ Some tests failed");
        std::process::exit(1);
    }
}
