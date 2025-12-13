// Common utilities and solution functions

import java.util.TreeMap

fun parseInput(input: String): List<String> = input.trim().lines()

data class SegmentMaps(
    val horizontal: TreeMap<Long, MutableList<Pair<Long, Long>>>,
    val vertical: TreeMap<Long, MutableList<Pair<Long, Long>>>
)

enum class Side {
  LEFT,
  RIGHT,
  TOP,
  BOTTOM
}

data class TaggedSegment(
    val start: Pair<Long, Long>,
    val end: Pair<Long, Long>,
    val insideSide: Side
)

fun buildSegmentMaps(coords: List<Pair<Long, Long>>, verbose: Boolean = false): SegmentMaps {
  val horizontal = TreeMap<Long, MutableList<Pair<Long, Long>>>()
  val vertical = TreeMap<Long, MutableList<Pair<Long, Long>>>()

  coords.indices.forEach { i ->
    val curr = coords[i]
    val next = coords[(i + 1) % coords.size]

    val (x1, y1) = curr
    val (x2, y2) = next

    when {
      x1 == x2 -> {
        // Vertical segment - same column (x)
        val minY = minOf(y1, y2)
        val maxY = maxOf(y1, y2)
        vertical.computeIfAbsent(x1) { mutableListOf() }.add(minY to maxY)
      }
      y1 == y2 -> {
        // Horizontal segment - same row (y)
        val minX = minOf(x1, x2)
        val maxX = maxOf(x1, x2)
        horizontal.computeIfAbsent(y1) { mutableListOf() }.add(minX to maxX)
      }
    }
  }

  if (verbose) {
    // Print statistics
    println("Horizontal segments: ${horizontal.values.sumOf { it.size }} total")
    println("  Rows with segments: ${horizontal.size}")
    println(
        "  Bucket sizes - min: ${horizontal.values.minOfOrNull { it.size } ?: 0}, max: ${horizontal.values.maxOfOrNull { it.size } ?: 0}, avg: ${if (horizontal.isNotEmpty()) horizontal.values.sumOf { it.size } / horizontal.size else 0}")

    println("Vertical segments: ${vertical.values.sumOf { it.size }} total")
    println("  Columns with segments: ${vertical.size}")
    println(
        "  Bucket sizes - min: ${vertical.values.minOfOrNull { it.size } ?: 0}, max: ${vertical.values.maxOfOrNull { it.size } ?: 0}, avg: ${if (vertical.isNotEmpty()) vertical.values.sumOf { it.size } / vertical.size else 0}")
  }

  return SegmentMaps(horizontal, vertical)
}

fun tagSegments(coords: List<Pair<Long, Long>>, verbose: Boolean = false): List<TaggedSegment> {
  // Determine if we're walking clockwise or counter-clockwise using shoelace formula
  val signedArea =
      coords.indices.sumOf { i ->
        val curr = coords[i]
        val next = coords[(i + 1) % coords.size]
        curr.first * next.second - next.first * curr.second
      }

  val insideToLeft = signedArea > 0
  if (verbose) {
    println("Signed area: $signedArea, walking counter-clockwise: $insideToLeft")
  }

  return coords.indices.mapNotNull { i ->
    val curr = coords[i]
    val next = coords[(i + 1) % coords.size]

    val (x1, y1) = curr
    val (x2, y2) = next

    when {
      x1 == x2 -> {
        // Vertical segment
        val goingDown = y2 > y1
        val insideSide = if (goingDown xor !insideToLeft) Side.RIGHT else Side.LEFT
        val (start, end) = if (y1 < y2) curr to next else next to curr
        TaggedSegment(start, end, insideSide)
      }
      y1 == y2 -> {
        // Horizontal segment
        val goingRight = x2 > x1
        val insideSide = if (goingRight xor !insideToLeft) Side.TOP else Side.BOTTOM
        val (start, end) = if (x1 < x2) curr to next else next to curr
        TaggedSegment(start, end, insideSide)
      }
      else -> null
    }
  }
}

fun isPointInPolygon(x: Long, y: Long, coords: List<Pair<Long, Long>>): Boolean {
  // Ray casting algorithm - count crossings of a horizontal ray to the right
  val crossings =
      coords.indices.count { i ->
        val curr = coords[i]
        val next = coords[(i + 1) % coords.size]

        val (x1, y1) = curr
        val (x2, y2) = next

        // Check if the edge crosses the horizontal ray to the right of the point
        ((y1 <= y && y < y2) || (y2 <= y && y < y1)) && x < x1 + (x2 - x1) * (y - y1) / (y2 - y1)
      }

  return crossings % 2 == 1
}

fun solvePart1(input: String): Long {
  val coords =
      parseInput(input).map {
        val (x, y) = it.split(",").map { it.trim().toLong() }
        x to y
      }

  return coords.maxOf { c1 ->
    coords.maxOf { c2 -> Math.abs(c1.first - c2.first + 1) * Math.abs(c1.second - c2.second + 1) }
  }
}

data class Rectangle(val left: Long, val right: Long, val top: Long, val bottom: Long) {
  val area: Long
    get() = (right - left + 1) * (bottom - top + 1)

  val centerX: Long
    get() = (left + right) / 2

  val centerY: Long
    get() = (top + bottom) / 2
}

fun hasSegmentCrossingEdges(rect: Rectangle, segmentMaps: SegmentMaps): Boolean {
  // Check vertical segments crossing horizontal edges
  val verticalCrossing =
      segmentMaps.vertical.entries
          .filter { (col, _) -> col in (rect.left + 1) until rect.right }
          .any { (_, segments) ->
            segments.any { (segMinY, segMaxY) ->
              (segMinY < rect.top && rect.top < segMaxY) ||
                  (segMinY < rect.bottom && rect.bottom < segMaxY)
            }
          }

  if (verticalCrossing) return true

  // Check horizontal segments crossing vertical edges
  return segmentMaps.horizontal.entries
      .filter { (row, _) -> row in (rect.top + 1) until rect.bottom }
      .any { (_, segments) ->
        segments.any { (segMinX, segMaxX) ->
          (segMinX < rect.left && rect.left < segMaxX) ||
              (segMinX < rect.right && rect.right < segMaxX)
        }
      }
}

fun hasCornerInside(rect: Rectangle, coords: List<Pair<Long, Long>>): Boolean =
    coords.any { (x, y) ->
      x in (rect.left + 1) until rect.right && y in (rect.top + 1) until rect.bottom
    }

fun hasCuttingSegment(rect: Rectangle, coords: List<Pair<Long, Long>>): Boolean {
  fun isOnBoundary(x: Long, y: Long) =
      x in rect.left..rect.right &&
          y in rect.top..rect.bottom &&
          (x == rect.left || x == rect.right || y == rect.top || y == rect.bottom)

  return coords.indices.any { i ->
    val (x1, y1) = coords[i]
    val (x2, y2) = coords[(i + 1) % coords.size]

    if (!isOnBoundary(x1, y1) || !isOnBoundary(x2, y2)) return@any false

    val isHorizontal = y1 == y2
    val isVertical = x1 == x2

    // Diagonal segment cuts through
    if (!isHorizontal && !isVertical) return@any true

    // Horizontal segment connecting left and right edges through middle
    if (isHorizontal && y1 != rect.top && y1 != rect.bottom) {
      return@any (x1 == rect.left && x2 == rect.right) || (x1 == rect.right && x2 == rect.left)
    }

    // Vertical segment connecting top and bottom edges through middle
    if (isVertical && x1 != rect.left && x1 != rect.right) {
      return@any (y1 == rect.top && y2 == rect.bottom) || (y1 == rect.bottom && y2 == rect.top)
    }

    false
  }
}

fun solvePart2(input: String, verbose: Boolean = false): Long {
  val coords =
      parseInput(input).map {
        val (x, y) = it.split(",").map { it.trim().toLong() }
        x to y
      }

  val segmentMaps = buildSegmentMaps(coords, verbose)
  val taggedSegments = tagSegments(coords, verbose)

  // Generate all potential rectangles from corner pairs
  val rectangles =
      coords.indices.flatMap { i ->
        ((i + 1) until coords.size).mapNotNull { j ->
          val (x1, y1) = coords[i]
          val (x2, y2) = coords[j]

          val rect =
              Rectangle(
                  left = minOf(x1, x2),
                  right = maxOf(x1, x2),
                  top = minOf(y1, y2),
                  bottom = maxOf(y1, y2))

          // Filter out degenerate rectangles
          if (rect.left == rect.right || rect.top == rect.bottom) null else rect
        }
      }

  // Filter valid rectangles
  val validRectangles =
      rectangles.filter { rect ->
        !hasSegmentCrossingEdges(rect, segmentMaps) &&
            !hasCornerInside(rect, coords) &&
            !hasCuttingSegment(rect, coords) &&
            isPointInPolygon(rect.centerX, rect.centerY, coords)
      }

  val maxRect = validRectangles.maxByOrNull { it.area }
  val maxArea = maxRect?.area ?: 0

  if (verbose) {
    println("Found ${validRectangles.size} valid rectangles")
    println("Max area: $maxArea")

    // Render visualization
    val maxRectPair = maxRect?.let { (it.left to it.top) to (it.right to it.bottom) }
    renderVisualization(coords, segmentMaps, maxRectPair, taggedSegments)
  }

  return maxArea
}
