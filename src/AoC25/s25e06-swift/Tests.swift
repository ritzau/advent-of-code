import Foundation

@main
enum TestRunner {
    static let sampleInput = """
    123 328  51 64
     45 64  387 23
      6 98  215 314
    *   +   *   +
    """

    static func main() {
        print("Running tests...")

        // Test Part 1
        let part1Result = solvePart1(sampleInput)
        if part1Result == 4_277_556 {
            print("✓ Part 1 test passed")
        } else {
            print("✗ Part 1 test failed: expected 0, got \(part1Result)")
            exit(1)
        }

        // Test Part 2
        let part2Result = solvePart2(sampleInput)
        if part2Result == 3_263_827 {
            print("✓ Part 2 test passed")
        } else {
            print("✗ Part 2 test failed: expected 0, got \(part2Result)")
            exit(1)
        }

        print("All tests passed!")
    }
}
