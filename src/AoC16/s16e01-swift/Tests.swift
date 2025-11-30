import Foundation

@main
enum Tests {
    static func main() {
        print("Running tests...")
        var allPassed = true

        // Part 1 tests
        let test1 = solvePart1("R2, L3")
        if test1 == 5 {
            print("✓ Part 1 test 1 passed")
        } else {
            print("✗ Part 1 test 1 failed: expected 5, got \(test1)")
            allPassed = false
        }

        let test2 = solvePart1("R2, R2, R2")
        if test2 == 2 {
            print("✓ Part 1 test 2 passed")
        } else {
            print("✗ Part 1 test 2 failed: expected 2, got \(test2)")
            allPassed = false
        }

        let test3 = solvePart1("R5, L5, R5, R3")
        if test3 == 12 {
            print("✓ Part 1 test 3 passed")
        } else {
            print("✗ Part 1 test 3 failed: expected 12, got \(test3)")
            allPassed = false
        }

        // Part 2 tests
        let test4 = solvePart2("R8, R4, R4, R8")
        if test4 == 4 {
            print("✓ Part 2 test 1 passed")
        } else {
            print("✗ Part 2 test 1 failed: expected 4, got \(test4)")
            allPassed = false
        }

        if allPassed {
            print("All tests passed!")
            exit(0)
        } else {
            print("Some tests failed!")
            exit(1)
        }
    }
}
