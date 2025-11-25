using NUnit.Framework;

namespace AoCSolution
{
    [TestFixture]
    public class Tests
    {
        [Test]
        public void TestPart1()
        {
            int result = Solution.SolvePart1("sample input");
            Assert.AreEqual(0, result, "Part 1 should return 0 for sample input");
        }

        [Test]
        public void TestPart2()
        {
            int result = Solution.SolvePart2("sample input");
            Assert.AreEqual(0, result, "Part 2 should return 0 for sample input");
        }
    }
}
