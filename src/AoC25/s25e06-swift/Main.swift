import Foundation

@main
enum Main {
    static func main() {
        var input = ""
        while let line = readLine() {
            input += line + "\n"
        }
        input = input.trimmingCharacters(in: .whitespacesAndNewlines)

        print("AoC Solution")
        print("============")

        // Part 1
        let start1 = Date()
        let result1 = solvePart1(input)
        let duration1 = Date().timeIntervalSince(start1) * 1000
        let expectedPart1 = 5_977_759_036_837
        let pass1 = result1 == expectedPart1

        let emoji1 = pass1 ? "✅" : "❌"
        print("Part 1: \(emoji1) \(result1) (expected: \(expectedPart1)) [\(String(format: "%.2f", duration1))ms]")

        // Part 2
        let start2 = Date()
        let result2 = solvePart2(input)
        let duration2 = Date().timeIntervalSince(start2) * 1000
        let expectedPart2 = 9_630_000_828_442
        let pass2 = result2 == expectedPart2

        let emoji2 = pass2 ? "✅" : "❌"
        print("Part 2: \(emoji2) \(result2) (expected: \(expectedPart2)) [\(String(format: "%.2f", duration2))ms]")

        print("Total: \(String(format: "%.2f", duration1 + duration2))ms")

        if pass1, pass2 {
            print("")
            print("🌟🌟 All tests passed!")
        } else {
            print("")
            print("❌ Some tests failed")
            exit(1)
        }
    }
}
