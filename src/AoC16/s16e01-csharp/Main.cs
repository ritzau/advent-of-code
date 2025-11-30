using System;
using System.Diagnostics;

namespace AoC16S16E01
{
    class Program
    {
        static void Main()
        {
            string input = Console.In.ReadToEnd().Trim();

            Console.WriteLine("AoC 2016 Day 1: No Time for a Taxicab");
            Console.WriteLine("======================================");

            // Part 1
            var sw1 = Stopwatch.StartNew();
            int result1 = Solution.SolvePart1(input);
            sw1.Stop();
            const int expectedPart1 = 300;
            bool pass1 = result1 == expectedPart1;

            string emoji1 = pass1 ? "✅" : "❌";
            Console.WriteLine($"Part 1: {emoji1} {result1} (expected: {expectedPart1}) [{sw1.Elapsed.TotalMilliseconds:F2}ms]");

            // Part 2
            var sw2 = Stopwatch.StartNew();
            int result2 = Solution.SolvePart2(input);
            sw2.Stop();
            const int expectedPart2 = 159;
            bool pass2 = result2 == expectedPart2;

            string emoji2 = pass2 ? "✅" : "❌";
            Console.WriteLine($"Part 2: {emoji2} {result2} (expected: {expectedPart2}) [{sw2.Elapsed.TotalMilliseconds:F2}ms]");

            Console.WriteLine($"Total: {(sw1.Elapsed.TotalMilliseconds + sw2.Elapsed.TotalMilliseconds):F2}ms");

            if (pass1 && pass2)
            {
                Console.WriteLine();
                Console.WriteLine("🌟🌟 All tests passed!");
            }
            else
            {
                Console.WriteLine();
                Console.WriteLine("❌ Some tests failed");
                Environment.Exit(1);
            }
        }
    }
}
