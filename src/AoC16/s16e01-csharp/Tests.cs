using NUnit.Framework;

namespace AoC16S16E01
{
    [TestFixture]
    public class Tests
    {
        [Test]
        public void Part1Sample1()
        {
            // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
            int result = Solution.SolvePart1("R2, L3");
            Assert.AreEqual(5, result, "Part 1 sample 1 should return 5");
        }

        [Test]
        public void Part1Sample2()
        {
            // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
            int result = Solution.SolvePart1("R2, R2, R2");
            Assert.AreEqual(2, result, "Part 1 sample 2 should return 2");
        }

        [Test]
        public void Part1Sample3()
        {
            // R5, L5, R5, R3 leaves you 12 blocks away
            int result = Solution.SolvePart1("R5, L5, R5, R3");
            Assert.AreEqual(12, result, "Part 1 sample 3 should return 12");
        }

        [Test]
        public void Part2Sample1()
        {
            // R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
            int result = Solution.SolvePart2("R8, R4, R4, R8");
            Assert.AreEqual(4, result, "Part 2 sample 1 should return 4");
        }
    }
}
