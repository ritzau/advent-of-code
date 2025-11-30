using System;

namespace AoC16S16E01
{
    class Part2
    {
        static void Main()
        {
            string input = Console.In.ReadToEnd().Trim();
            int result = Solution.SolvePart2(input);
            Console.WriteLine(result);
        }
    }
}
