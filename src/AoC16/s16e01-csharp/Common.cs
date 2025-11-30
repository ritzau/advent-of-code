using System;
using System.Collections.Generic;
using System.Linq;

namespace AoC16S16E01
{
    public enum Direction
    {
        North,
        East,
        South,
        West
    }

    public static class DirectionExtensions
    {
        public static Direction TurnRight(this Direction dir)
        {
            return dir switch
            {
                Direction.North => Direction.East,
                Direction.East => Direction.South,
                Direction.South => Direction.West,
                Direction.West => Direction.North,
                _ => throw new ArgumentException($"Invalid direction: {dir}")
            };
        }

        public static Direction TurnLeft(this Direction dir)
        {
            return dir switch
            {
                Direction.North => Direction.West,
                Direction.West => Direction.South,
                Direction.South => Direction.East,
                Direction.East => Direction.North,
                _ => throw new ArgumentException($"Invalid direction: {dir}")
            };
        }

        public static (int dx, int dy) Delta(this Direction dir)
        {
            return dir switch
            {
                Direction.North => (0, 1),
                Direction.East => (1, 0),
                Direction.South => (0, -1),
                Direction.West => (-1, 0),
                _ => throw new ArgumentException($"Invalid direction: {dir}")
            };
        }
    }

    public class Instruction
    {
        public char Turn { get; set; }
        public int Blocks { get; set; }
    }

    public static class Solution
    {
        public static List<Instruction> ParseInput(string input)
        {
            return input.Trim()
                .Split(", ")
                .Select(s => new Instruction
                {
                    Turn = s[0],
                    Blocks = int.Parse(s.Substring(1))
                })
                .ToList();
        }

        public static int SolvePart1(string input)
        {
            var instructions = ParseInput(input);

            int x = 0;
            int y = 0;
            Direction direction = Direction.North;

            foreach (var instruction in instructions)
            {
                direction = instruction.Turn switch
                {
                    'R' => direction.TurnRight(),
                    'L' => direction.TurnLeft(),
                    _ => throw new ArgumentException($"Invalid turn: {instruction.Turn}")
                };

                var (dx, dy) = direction.Delta();
                x += dx * instruction.Blocks;
                y += dy * instruction.Blocks;
            }

            return Math.Abs(x) + Math.Abs(y);
        }

        public static int SolvePart2(string input)
        {
            var instructions = ParseInput(input);

            int x = 0;
            int y = 0;
            Direction direction = Direction.North;
            var visited = new HashSet<(int, int)>();

            visited.Add((0, 0));

            foreach (var instruction in instructions)
            {
                direction = instruction.Turn switch
                {
                    'R' => direction.TurnRight(),
                    'L' => direction.TurnLeft(),
                    _ => throw new ArgumentException($"Invalid turn: {instruction.Turn}")
                };

                var (dx, dy) = direction.Delta();

                for (int i = 0; i < instruction.Blocks; i++)
                {
                    x += dx;
                    y += dy;

                    if (!visited.Add((x, y)))
                    {
                        return Math.Abs(x) + Math.Abs(y);
                    }
                }
            }

            return 0;
        }
    }
}
