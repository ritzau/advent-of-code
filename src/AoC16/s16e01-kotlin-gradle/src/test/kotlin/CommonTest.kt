import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals

class CommonTest {
    @Test
    fun `part1 sample 1 - R2 L3 is 5 blocks away`() {
        // Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
        val result = solvePart1("R2, L3")
        assertEquals(5, result, "SolvePart1(\"R2, L3\") should equal 5")
    }

    @Test
    fun `part1 sample 2 - R2 R2 R2 is 2 blocks away`() {
        // R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
        val result = solvePart1("R2, R2, R2")
        assertEquals(2, result, "SolvePart1(\"R2, R2, R2\") should equal 2")
    }

    @Test
    fun `part1 sample 3 - R5 L5 R5 R3 is 12 blocks away`() {
        // R5, L5, R5, R3 leaves you 12 blocks away
        val result = solvePart1("R5, L5, R5, R3")
        assertEquals(12, result, "SolvePart1(\"R5, L5, R5, R3\") should equal 12")
    }

    @Test
    fun `part2 sample 1 - R8 R4 R4 R8 first revisit is 4 blocks away`() {
        // R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
        val result = solvePart2("R8, R4, R4, R8")
        assertEquals(4, result, "SolvePart2(\"R8, R4, R4, R8\") should equal 4")
    }
}
