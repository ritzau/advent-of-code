using System;

namespace AoCSolution
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
