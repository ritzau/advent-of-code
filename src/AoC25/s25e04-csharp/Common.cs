using System;
using System.Linq;

namespace AoCSolution
{
    public static class Solution
    {
        public static char[][] ParseInput(string input)
        {
            return input.Split('\n').Select(line => line.ToCharArray()).ToArray();
        }

        public static int SolvePart1(string input)
        {
            var lines = ParseInput(input);

            var accessibleCount = 0;

            for (var row = 0; row < lines.Length; row++)
            {
                for (var col = 0; col < lines[row].Length; col++)
                {
                   if (IsAccessible(lines, row, col))
                   {
                       accessibleCount++;
                   }
                }
            }

            return accessibleCount;
        }

        public static int SolvePart2(string input)
        {
            var lines = ParseInput(input);
            var fifoQueue = new System.Collections.Generic.Queue<(int, int)>();
            var removedCount = 0;

            for (var rowIndex = 0; rowIndex < lines.Length; rowIndex++)
            {
                for (var colIndex = 0; colIndex < lines[rowIndex].Length; colIndex++)
                {
                    if (HasPaperRoll(lines, rowIndex, colIndex))
                    {
                        fifoQueue.Enqueue((rowIndex, colIndex));
                    }
                }
            }

            while (fifoQueue.Count > 0)
            {
                var (row, col) = fifoQueue.Dequeue();

                if (IsAccessible(lines, row, col))
                {
                    lines[row][col] = 'x';
                    removedCount++;

                    foreach (var (dy, dx) in neighborDirections)
                    {
                        int newRow = row + dy;
                        int newCol = col + dx;

                        if (newRow >= 0 && newRow < lines.Length &&
                            newCol >= 0 && newCol < lines[newRow].Length)
                        {
                            if (HasPaperRoll(lines, newRow, newCol))
                            {
                                fifoQueue.Enqueue((newRow, newCol));
                            }
                        }
                    }
                }
            }
            return removedCount;
        }

        static (int, int)[] neighborDirections = new (int, int)[]
        {
            (-1, 0), (1, 0), (0, -1), (0, 1),
            (-1, -1), (-1, 1), (1, -1), (1, 1)
        };

        static bool IsAccessible(char[][] lines, int row, int col)
        {
            if (HasPaperRoll(lines, row, col) == false)
                return false;

            int neighborCount = 0;
            foreach (var (dy, dx) in neighborDirections)
            {
                int newRow = row + dy;
                int newCol = col + dx;

                if (newRow >= 0 && newRow < lines.Length &&
                    newCol >= 0 && newCol < lines[newRow].Length)
                {
                    if (HasPaperRoll(lines, newRow, newCol))
                    {
                        neighborCount++;
                    }
                }
            }
            return neighborCount < 4;
        }

        static bool HasPaperRoll(char[][] lines, int row, int col)
        {
            return lines[row][col] == '@';
        }
    }
}
