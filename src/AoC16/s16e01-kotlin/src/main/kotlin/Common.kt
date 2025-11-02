enum class Direction {
    NORTH,
    EAST,
    SOUTH,
    WEST,
    ;

    fun turnRight(): Direction =
        when (this) {
            NORTH -> EAST
            EAST -> SOUTH
            SOUTH -> WEST
            WEST -> NORTH
        }

    fun turnLeft(): Direction =
        when (this) {
            NORTH -> WEST
            WEST -> SOUTH
            SOUTH -> EAST
            EAST -> NORTH
        }

    fun delta(): Pair<Int, Int> =
        when (this) {
            NORTH -> Pair(0, 1)
            EAST -> Pair(1, 0)
            SOUTH -> Pair(0, -1)
            WEST -> Pair(-1, 0)
        }
}

data class Instruction(
    val turn: Char,
    val blocks: Int,
)

fun parseInput(input: String): List<Instruction> =
    input
        .trim()
        .split(", ")
        .filter { it.isNotEmpty() }
        .map { part ->
            val turn = part[0]
            val blocks = part.substring(1).toInt()
            Instruction(turn, blocks)
        }

fun solvePart1(input: String): Int {
    val instructions = parseInput(input)
    var x = 0
    var y = 0
    var direction = Direction.NORTH

    for (instruction in instructions) {
        direction =
            when (instruction.turn) {
                'R' -> direction.turnRight()
                'L' -> direction.turnLeft()
                else -> direction
            }

        val (dx, dy) = direction.delta()
        x += dx * instruction.blocks
        y += dy * instruction.blocks
    }

    return kotlin.math.abs(x) + kotlin.math.abs(y)
}

fun solvePart2(input: String): Int {
    val instructions = parseInput(input)
    var x = 0
    var y = 0
    var direction = Direction.NORTH
    val visited = mutableSetOf<Pair<Int, Int>>()
    visited.add(Pair(0, 0))

    for (instruction in instructions) {
        direction =
            when (instruction.turn) {
                'R' -> direction.turnRight()
                'L' -> direction.turnLeft()
                else -> direction
            }

        val (dx, dy) = direction.delta()

        repeat(instruction.blocks) {
            x += dx
            y += dy

            val pos = Pair(x, y)
            if (pos in visited) {
                return kotlin.math.abs(x) + kotlin.math.abs(y)
            }
            visited.add(pos)
        }
    }

    return 0
}
