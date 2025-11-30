using System;

namespace AoC16S16E01
{
    class Part1
    {
        static void Main()
        {
            string input = Console.In.ReadToEnd().Trim();
            int result = Solution.SolvePart1(input);
            Console.WriteLine(result);
        }
    }
}
