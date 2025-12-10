// Common utilities and solution functions

fun parseInput(input: String): List<String> = input.trim().lines()

fun solvePart1(input: String): Long {
  val lines = parseInput(input)
  val coords = lines.map {
    val (x, y) = it.split(",").map { it.trim().toLong() }
    x to y
  }

  // val maxRow = coords.maxOf { it.second }
  // val maxCol = coords.maxOf { it.first }

  // var firstTileInRow = MutableList<Int?>(maxRow + 1) { null }
  // val lastTileInRow = MutableList<Int?>(maxRow + 1) { null }
  // val firstTileInCol = MutableList<Int?>(maxCol + 1) { null }
  // val lastTileInCol = MutableList<Int?>(maxCol + 1) { null }
  // for ((x, y) in coords) {
  //   if (firstTileInRow[y] == null || x < firstTileInRow[y]!!) {
  //     firstTileInRow[y] = x
  //   }
  //   if (lastTileInRow[y] == null || x > lastTileInRow[y]!!) {
  //     lastTileInRow[y] = x
  //   }
  //   if (firstTileInCol[x] == null || y < firstTileInCol[x]!!) {
  //     firstTileInCol[x] = y
  //   }
  //   if (lastTileInCol[x] == null || y > lastTileInCol[x]!!) {
  //     lastTileInCol[x] = y
  //   }
  // }

  // val topLeft = firstTileInCol.withIndex().map { (col, row) ->
  //   if (row == null) null else col to row
  // }

  // val bottomRight = lastTileInCol.withIndex().map { (col, row) ->
  //   if (row == null) null else col to row
  // }

  // val bottomLeft = firstTileInRow.withIndex().map { (row, col) ->
  //   if (col == null) null else col to row
  // }
  // val topRight = lastTileInRow.withIndex().map { (row, col) ->
  //   if (col == null) null else col to row
  // }

  var maxArea = 0L

  for (c1 in coords) {
    for (c2 in coords) {
      val area = Math.abs(c1.first - c2.first + 1) * Math.abs(c1.second - c2.second + 1)
      if (area > maxArea) {
        maxArea = area
      }
    }
  }

  // for (tl in topLeft) {
  //   for (br in bottomRight) {
  //     if (tl != null && br != null) {
  //       val area = (br.first - tl.first + 1) * (br.second - tl.second + 1)
  //       if (area > maxArea) {
  //         maxArea = area
  //       }
  //     }
  //   }
  // }

  // for (bl in bottomLeft) {
  //   for (tr in topRight) {
  //     if (bl != null && tr != null) {
  //       val area = (tr.first - bl.first + 1) * (bl.second - tr.second + 1)
  //       if (area > maxArea) {
  //         maxArea = area
  //       }
  //     }
  //   }
  // }

  return maxArea
}

fun solvePart2(input: String): Long {
  val lines = parseInput(input)
  // TODO: Implement solution for part 2
  return 0
}
