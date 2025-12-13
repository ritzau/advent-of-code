// Common utilities and solution functions

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.util.TreeMap
import javax.imageio.ImageIO

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

fun buildSegmentMaps(coords: List<Pair<Long, Long>>): SegmentMaps {
  val horizontal = TreeMap<Long, MutableList<Pair<Long, Long>>>()
  val vertical = TreeMap<Long, MutableList<Pair<Long, Long>>>()

  for (i in coords.indices) {
    val curr = coords[i]
    val next = coords[(i + 1) % coords.size]

    val x1 = curr.first
    val y1 = curr.second
    val x2 = next.first
    val y2 = next.second

    if (x1 == x2) {
      // Vertical segment - same column (x)
      val col = x1
      val minY = minOf(y1, y2)
      val maxY = maxOf(y1, y2)
      vertical.computeIfAbsent(col) { mutableListOf() }.add(minY to maxY)
    } else if (y1 == y2) {
      // Horizontal segment - same row (y)
      val row = y1
      val minX = minOf(x1, x2)
      val maxX = maxOf(x1, x2)
      horizontal.computeIfAbsent(row) { mutableListOf() }.add(minX to maxX)
    }
  }

  // Print statistics
  println("Horizontal segments: ${horizontal.values.sumOf { it.size }} total")
  println("  Rows with segments: ${horizontal.size}")
  println(
      "  Bucket sizes - min: ${horizontal.values.minOfOrNull { it.size } ?: 0}, max: ${horizontal.values.maxOfOrNull { it.size } ?: 0}, avg: ${if (horizontal.isNotEmpty()) horizontal.values.sumOf { it.size } / horizontal.size else 0}")

  println("Vertical segments: ${vertical.values.sumOf { it.size }} total")
  println("  Columns with segments: ${vertical.size}")
  println(
      "  Bucket sizes - min: ${vertical.values.minOfOrNull { it.size } ?: 0}, max: ${vertical.values.maxOfOrNull { it.size } ?: 0}, avg: ${if (vertical.isNotEmpty()) vertical.values.sumOf { it.size } / vertical.size else 0}")

  return SegmentMaps(horizontal, vertical)
}

fun tagSegments(coords: List<Pair<Long, Long>>): List<TaggedSegment> {
  // Determine if we're walking clockwise or counter-clockwise
  // Use shoelace formula to compute signed area
  var signedArea = 0L
  for (i in coords.indices) {
    val curr = coords[i]
    val next = coords[(i + 1) % coords.size]
    signedArea += curr.first * next.second - next.first * curr.second
  }

  // If signedArea > 0, we're walking counter-clockwise, inside is to the left
  // If signedArea < 0, we're walking clockwise, inside is to the right
  val insideToLeft = signedArea > 0

  println("Signed area: $signedArea, walking counter-clockwise: $insideToLeft")

  val tagged = mutableListOf<TaggedSegment>()

  for (i in coords.indices) {
    val curr = coords[i]
    val next = coords[(i + 1) % coords.size]

    val x1 = curr.first
    val y1 = curr.second
    val x2 = next.first
    val y2 = next.second

    if (x1 == x2) {
      // Vertical segment from curr to next
      val goingDown = y2 > y1
      // When going down and inside is to left, inside is on LEFT
      // When going down and inside is to right, inside is on RIGHT
      // When going up and inside is to left, inside is on RIGHT
      // When going up and inside is to right, inside is on LEFT
      val insideSide =
          if (goingDown) {
            if (insideToLeft) Side.LEFT else Side.RIGHT
          } else {
            if (insideToLeft) Side.RIGHT else Side.LEFT
          }

      // Normalize to have min Y first
      val (start, end) =
          if (y1 < y2) {
            curr to next
          } else {
            next to curr
          }
      tagged.add(TaggedSegment(start, end, insideSide))
    } else if (y1 == y2) {
      // Horizontal segment from curr to next
      val goingRight = x2 > x1
      // When going right and inside is to left, inside is on BOTTOM (below)
      // When going right and inside is to right, inside is on TOP (above)
      // When going left and inside is to left, inside is on TOP (above)
      // When going left and inside is to right, inside is on BOTTOM (below)
      val insideSide =
          if (goingRight) {
            if (insideToLeft) Side.BOTTOM else Side.TOP
          } else {
            if (insideToLeft) Side.TOP else Side.BOTTOM
          }

      // Normalize to have min X first
      val (start, end) =
          if (x1 < x2) {
            curr to next
          } else {
            next to curr
          }
      tagged.add(TaggedSegment(start, end, insideSide))
    }
  }

  return tagged
}

fun isPointInPolygon(x: Long, y: Long, coords: List<Pair<Long, Long>>): Boolean {
  // Ray casting algorithm - count how many times a ray from the point crosses the polygon boundary
  var crossings = 0

  for (i in coords.indices) {
    val curr = coords[i]
    val next = coords[(i + 1) % coords.size]

    val x1 = curr.first
    val y1 = curr.second
    val x2 = next.first
    val y2 = next.second

    // Check if the edge crosses the horizontal ray to the right of the point
    if ((y1 <= y && y < y2) || (y2 <= y && y < y1)) {
      // Edge crosses the horizontal line at y
      // Calculate x coordinate where edge crosses y
      val xCross = x1 + (x2 - x1) * (y - y1) / (y2 - y1)
      if (x < xCross) {
        crossings++
      }
    }
  }

  return crossings % 2 == 1
}

fun solvePart1(input: String): Long {
  val lines = parseInput(input)
  val coords =
      lines.map {
        val (x, y) = it.split(",").map { it.trim().toLong() }
        x to y
      }

  var maxArea = 0L

  for (c1 in coords) {
    for (c2 in coords) {
      val area = Math.abs(c1.first - c2.first + 1) * Math.abs(c1.second - c2.second + 1)
      if (area > maxArea) {
        maxArea = area
      }
    }
  }

  return maxArea
}

fun solvePart2(input: String): Long {
  val lines = parseInput(input)
  val coords =
      lines.map {
        val (x, y) = it.split(",").map { it.trim().toLong() }
        x to y
      }

  val segmentMaps = buildSegmentMaps(coords)
  val taggedSegments = tagSegments(coords)

  var maxArea = 0L
  var rectangleCount = 0
  var maxRect: Pair<Pair<Long, Long>, Pair<Long, Long>>? = null

  // Try all pairs of corners
  for (i in coords.indices) {
    for (j in i + 1 until coords.size) {
      val c1 = coords[i]
      val c2 = coords[j]

      // Form a rectangle
      val left = minOf(c1.first, c2.first)
      val right = maxOf(c1.first, c2.first)
      val top = minOf(c1.second, c2.second)
      val bottom = maxOf(c1.second, c2.second)

      if (left == right || top == bottom) continue // Not a rectangle

      // Check if any segments cross through the rectangle's edges
      var isInvalid = false

      // Check vertical segments crossing horizontal edges
      for ((col, segments) in segmentMaps.vertical.entries) {
        if (col <= left || col >= right) continue

        for ((segMinY, segMaxY) in segments) {
          // Check if crosses top edge
          if (segMinY < top && top < segMaxY) {
            isInvalid = true
            break
          }
          // Check if crosses bottom edge
          if (segMinY < bottom && bottom < segMaxY) {
            isInvalid = true
            break
          }
        }
        if (isInvalid) break
      }

      // Check horizontal segments crossing vertical edges
      if (!isInvalid) {
        for ((row, segments) in segmentMaps.horizontal.entries) {
          if (row <= top || row >= bottom) continue

          for ((segMinX, segMaxX) in segments) {
            // Check if crosses left edge
            if (segMinX < left && left < segMaxX) {
              isInvalid = true
              break
            }
            // Check if crosses right edge
            if (segMinX < right && right < segMaxX) {
              isInvalid = true
              break
            }
          }
          if (isInvalid) break
        }
      }

      // Check if any polygon corners are strictly inside the rectangle
      // (not on the edges)
      if (!isInvalid) {
        for ((x, y) in coords) {
          if (x > left && x < right && y > top && y < bottom) {
            // Polygon corner is strictly inside rectangle - invalid!
            isInvalid = true
            break
          }
        }
      }

      // Check for diagonal segments that cut through the rectangle
      // A segment is invalid if both endpoints are on the boundary but the segment
      // passes through the interior (not along an edge)
      if (!isInvalid) {
        for (i in coords.indices) {
          val curr = coords[i]
          val next = coords[(i + 1) % coords.size]

          val x1 = curr.first
          val y1 = curr.second
          val x2 = next.first
          val y2 = next.second

          // Skip if not both endpoints on rectangle boundary
          val p1OnBoundary =
              x1 >= left &&
                  x1 <= right &&
                  y1 >= top &&
                  y1 <= bottom &&
                  (x1 == left || x1 == right || y1 == top || y1 == bottom)
          val p2OnBoundary =
              x2 >= left &&
                  x2 <= right &&
                  y2 >= top &&
                  y2 <= bottom &&
                  (x2 == left || x2 == right || y2 == top || y2 == bottom)

          if (p1OnBoundary && p2OnBoundary) {
            // Segment with both endpoints on boundary
            // It's invalid if it's NOT axis-aligned OR if it crosses opposite edges
            val isHorizontal = (y1 == y2)
            val isVertical = (x1 == x2)

            if (!isHorizontal && !isVertical) {
              // Diagonal segment - invalid!
              isInvalid = true
              break
            }

            // Check if horizontal segment connects left and right edges (not along top/bottom)
            if (isHorizontal && y1 != top && y1 != bottom) {
              if ((x1 == left && x2 == right) || (x1 == right && x2 == left)) {
                isInvalid = true
                break
              }
            }

            // Check if vertical segment connects top and bottom edges (not along left/right)
            if (isVertical && x1 != left && x1 != right) {
              if ((y1 == top && y2 == bottom) || (y1 == bottom && y2 == top)) {
                isInvalid = true
                break
              }
            }
          }
        }
      }

      if (!isInvalid) {
        // Check if the rectangle center is inside the polygon
        val centerX = (left + right) / 2
        val centerY = (top + bottom) / 2

        if (isPointInPolygon(centerX, centerY, coords)) {
          // Valid rectangle!
          val area = (right - left + 1) * (bottom - top + 1)
          rectangleCount++
          if (area > maxArea) {
            maxArea = area
            maxRect = (left to top) to (right to bottom)
          }
        }
      }
    }
  }

  println("Found $rectangleCount valid rectangles")
  println("Max area: $maxArea")

  // Render visualization
  renderVisualization(coords, segmentMaps, maxRect, taggedSegments)

  return maxArea
}

fun renderVisualization(
    coords: List<Pair<Long, Long>>,
    segmentMaps: SegmentMaps,
    maxRect: Pair<Pair<Long, Long>, Pair<Long, Long>>?,
    taggedSegments: List<TaggedSegment>
) {
  val minX = coords.minOf { it.first }
  val maxX = coords.maxOf { it.first }
  val minY = coords.minOf { it.second }
  val maxY = coords.maxOf { it.second }

  val width = (maxX - minX + 1).toInt()
  val height = (maxY - minY + 1).toInt()

  // Scale if too large
  val scale =
      if (width > 2000 || height > 2000) {
        minOf(2000.0 / width, 2000.0 / height)
      } else {
        1.0
      }

  val scaledWidth = (width * scale).toInt()
  val scaledHeight = (height * scale).toInt()

  val img = BufferedImage(scaledWidth, scaledHeight, BufferedImage.TYPE_INT_RGB)
  val g = img.createGraphics()

  // White background
  g.color = Color.WHITE
  g.fillRect(0, 0, scaledWidth, scaledHeight)

  // Draw horizontal segments in blue
  g.color = Color.BLUE
  for ((row, segments) in segmentMaps.horizontal.entries) {
    for ((minXSeg, maxXSeg) in segments) {
      val x1 = ((minXSeg - minX) * scale).toInt()
      val x2 = ((maxXSeg - minX) * scale).toInt()
      val y = ((row - minY) * scale).toInt()
      g.drawLine(x1, y, x2, y)
    }
  }

  // Draw vertical segments in red
  g.color = Color.RED
  for ((col, segments) in segmentMaps.vertical.entries) {
    for ((minYSeg, maxYSeg) in segments) {
      val x = ((col - minX) * scale).toInt()
      val y1 = ((minYSeg - minY) * scale).toInt()
      val y2 = ((maxYSeg - minY) * scale).toInt()
      g.drawLine(x, y1, x, y2)
    }
  }

  // Draw corners in green
  g.color = Color.GREEN
  for ((x, y) in coords) {
    val px = ((x - minX) * scale).toInt()
    val py = ((y - minY) * scale).toInt()
    g.fillOval(px - 2, py - 2, 4, 4)
  }

  // Draw max rectangle in yellow/orange with transparency
  if (maxRect != null) {
    val (topLeft, bottomRight) = maxRect
    val (rectLeft, rectTop) = topLeft
    val (rectRight, rectBottom) = bottomRight

    val x1 = ((rectLeft - minX) * scale).toInt()
    val y1 = ((rectTop - minY) * scale).toInt()
    val x2 = ((rectRight - minX) * scale).toInt()
    val y2 = ((rectBottom - minY) * scale).toInt()

    g.color = Color(255, 200, 0, 128) // Semi-transparent orange
    g.fillRect(x1, y1, x2 - x1 + 1, y2 - y1 + 1)

    // Draw outline in solid orange
    g.color = Color(255, 150, 0, 255)
    g.drawRect(x1, y1, x2 - x1, y2 - y1)
  }

  // Draw dots indicating the inside side of each segment
  for (segment in taggedSegments) {
    val (x1, y1) = segment.start
    val (x2, y2) = segment.end

    // Calculate midpoint
    val midX = (x1 + x2) / 2.0
    val midY = (y1 + y2) / 2.0

    // Determine if this is a vertical or horizontal segment
    val isVertical = x1 == x2
    val isHorizontal = y1 == y2

    // Set color based on segment type
    g.color = if (isVertical) Color.RED else Color.BLUE

    // Offset by 10 pixels in the direction of the inside
    val offsetPixels = 10.0 / scale
    val (dotX, dotY) =
        when (segment.insideSide) {
          Side.LEFT -> midX - offsetPixels to midY
          Side.RIGHT -> midX + offsetPixels to midY
          Side.TOP -> midX to midY - offsetPixels
          Side.BOTTOM -> midX to midY + offsetPixels
        }

    val px = ((dotX - minX) * scale).toInt()
    val py = ((dotY - minY) * scale).toInt()
    g.fillOval(px - 3, py - 3, 6, 6)
  }

  g.dispose()

  val outputFile = File("polygon_visualization.png")
  ImageIO.write(img, "png", outputFile)
  println("Visualization saved to: ${outputFile.absolutePath}")
}
