// Visualization utilities for rendering polygons and rectangles

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

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
