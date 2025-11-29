import Foundation

func main() {
    print("Running tests...")

    // Test Part 1
    let part1Result = solvePart1("sample input")
    if part1Result == 0 {
        print("✓ Part 1 test passed")
    } else {
        print("✗ Part 1 test failed: expected 0, got \(part1Result)")
        exit(1)
    }

    // Test Part 2
    let part2Result = solvePart2("sample input")
    if part2Result == 0 {
        print("✓ Part 2 test passed")
    } else {
        print("✗ Part 2 test failed: expected 0, got \(part2Result)")
        exit(1)
    }

    print("All tests passed!")
}

main()
