using NUnit.Framework;

namespace AoCSolution
{
    [TestFixture]
    public class Tests
    {
        string sampleInput = """
            ..@@.@@@@.
            @@@.@.@.@@
            @@@@@.@.@@
            @.@@@@..@.
            @@.@@@@.@@
            .@@@@@@@.@
            .@.@.@.@@@
            @.@@@.@@@@
            .@@@@@@@@.
            @.@.@@@.@.
            """;

        [Test]
        public void TestPart1()
        {
            int result = Solution.SolvePart1(sampleInput);
            Assert.AreEqual(13, result, "Part 1 should return 0 for sample input");
        }

        [Test]
        public void TestPart2()
        {
            int result = Solution.SolvePart2(sampleInput);
            Assert.AreEqual(43, result, "Part 2 should return 43 for sample input");
        }
    }
}
