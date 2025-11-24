using System;

namespace AoCSolution
{
    class Tests
    {
        static void Main()
        {
            Console.WriteLine("Running tests...");

            // Test Part 1
            int part1Result = Solution.SolvePart1("sample input");
            if (part1Result == 0)
            {
                Console.WriteLine("✓ Part 1 test passed");
            }
            else
            {
                Console.WriteLine($"✗ Part 1 test failed: expected 0, got {part1Result}");
                Environment.Exit(1);
            }

            // Test Part 2
            int part2Result = Solution.SolvePart2("sample input");
            if (part2Result == 0)
            {
                Console.WriteLine("✓ Part 2 test passed");
            }
            else
            {
                Console.WriteLine($"✗ Part 2 test failed: expected 0, got {part2Result}");
                Environment.Exit(1);
            }

            Console.WriteLine("All tests passed!");
        }
    }
}
