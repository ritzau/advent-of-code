// Interactive debugger for visualizing rectangle candidates

import java.awt.*
import java.awt.event.*
import javax.swing.*

data class RectCandidate(
    val i: Int,
    val j: Int,
    val c1: Pair<Long, Long>,
    val c2: Pair<Long, Long>,
    val left: Long,
    val right: Long,
    val top: Long,
    val bottom: Long,
    val area: Long,
    val hasIntersection: Boolean,
    val centerInside: Boolean,
    val isValid: Boolean
)

class PolygonVisualizer(
    val coords: List<Pair<Long, Long>>,
    val segmentMaps: SegmentMaps,
    val candidates: List<RectCandidate>
) : JPanel() {

  var currentIndex = 0

  val minX = coords.minOf { it.first }
  val maxX = coords.maxOf { it.first }
  val minY = coords.minOf { it.second }
  val maxY = coords.maxOf { it.second }

  val margin = 40

  override fun paintComponent(g: Graphics) {
    super.paintComponent(g)
    val g2 = g as Graphics2D
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    val w = width - 2 * margin
    val h = height - 2 * margin

    val scaleX = w.toDouble() / (maxX - minX + 1)
    val scaleY = h.toDouble() / (maxY - minY + 1)
    val scale = minOf(scaleX, scaleY)

    fun toScreenX(x: Long): Int = margin + ((x - minX) * scale).toInt()
    fun toScreenY(y: Long): Int = margin + ((y - minY) * scale).toInt()

    // Draw current rectangle if exists
    if (currentIndex < candidates.size) {
      val rect = candidates[currentIndex]
      val x1 = toScreenX(rect.left)
      val y1 = toScreenY(rect.top)
      val x2 = toScreenX(rect.right)
      val y2 = toScreenY(rect.bottom)

      g2.color = if (rect.isValid) Color(0, 255, 0, 50) else Color(255, 0, 0, 50)
      g2.fillRect(x1, y1, x2 - x1, y2 - y1)

      g2.color = if (rect.isValid) Color.GREEN else Color.RED
      g2.stroke = BasicStroke(2f)
      g2.drawRect(x1, y1, x2 - x1, y2 - y1)

      // Highlight the two corners used
      g2.color = Color.MAGENTA
      g2.fillOval(toScreenX(rect.c1.first) - 5, toScreenY(rect.c1.second) - 5, 10, 10)
      g2.fillOval(toScreenX(rect.c2.first) - 5, toScreenY(rect.c2.second) - 5, 10, 10)
    }

    // Draw horizontal segments in blue
    g2.color = Color.BLUE
    g2.stroke = BasicStroke(2f)
    for ((row, segments) in segmentMaps.horizontal.entries) {
      for ((minXSeg, maxXSeg) in segments) {
        val x1 = toScreenX(minXSeg)
        val x2 = toScreenX(maxXSeg)
        val y = toScreenY(row)
        g2.drawLine(x1, y, x2, y)
      }
    }

    // Draw vertical segments in red
    g2.color = Color.RED
    for ((col, segments) in segmentMaps.vertical.entries) {
      for ((minYSeg, maxYSeg) in segments) {
        val x = toScreenX(col)
        val y1 = toScreenY(minYSeg)
        val y2 = toScreenY(maxYSeg)
        g2.drawLine(x, y1, x, y2)
      }
    }

    // Draw corners in black
    g2.color = Color.BLACK
    for ((x, y) in coords) {
      val px = toScreenX(x)
      val py = toScreenY(y)
      g2.fillOval(px - 3, py - 3, 6, 6)
    }
  }

  override fun getPreferredSize(): Dimension = Dimension(800, 600)
}

fun debugInteractive(input: String) {
  val lines = parseInput(input)
  val coords =
      lines.map {
        val (x, y) = it.split(",").map { it.trim().toLong() }
        x to y
      }

  val segmentMaps = buildSegmentMaps(coords)
  val candidates = mutableListOf<RectCandidate>()

  // Collect all rectangle candidates
  for (i in coords.indices) {
    for (j in i + 1 until coords.size) {
      val c1 = coords[i]
      val c2 = coords[j]

      val left = minOf(c1.first, c2.first)
      val right = maxOf(c1.first, c2.first)
      val top = minOf(c1.second, c2.second)
      val bottom = maxOf(c1.second, c2.second)

      if (left == right || top == bottom) continue

      var hasIntersection = false

      // Check vertical segments crossing horizontal edges
      for ((col, segments) in segmentMaps.vertical.entries) {
        if (col <= left || col >= right) continue
        for ((segMinY, segMaxY) in segments) {
          if (segMinY < top && top < segMaxY) {
            hasIntersection = true
            break
          }
          if (segMinY < bottom && bottom < segMaxY) {
            hasIntersection = true
            break
          }
        }
        if (hasIntersection) break
      }

      // Check horizontal segments crossing vertical edges
      if (!hasIntersection) {
        for ((row, segments) in segmentMaps.horizontal.entries) {
          if (row <= top || row >= bottom) continue
          for ((segMinX, segMaxX) in segments) {
            if (segMinX < left && left < segMaxX) {
              hasIntersection = true
              break
            }
            if (segMinX < right && right < segMaxX) {
              hasIntersection = true
              break
            }
          }
          if (hasIntersection) break
        }
      }

      // Check if any polygon corners are strictly inside the rectangle
      var hasCornerInside = false
      if (!hasIntersection) {
        for ((x, y) in coords) {
          if (x > left && x < right && y > top && y < bottom) {
            hasCornerInside = true
            break
          }
        }
      }

      val centerX = (left + right) / 2
      val centerY = (top + bottom) / 2
      val centerInside = isPointInPolygon(centerX, centerY, coords)

      val area = (right - left + 1) * (bottom - top + 1)
      val isValid = !hasIntersection && !hasCornerInside && centerInside

      candidates.add(
          RectCandidate(
              i, j, c1, c2, left, right, top, bottom, area, hasIntersection, centerInside, isValid))
    }
  }

  // Sort by area descending
  candidates.sortByDescending { it.area }

  val frame = JFrame("Rectangle Debug Visualizer")
  val visualizer = PolygonVisualizer(coords, segmentMaps, candidates)

  val infoLabel = JLabel()

  fun updateInfo() {
    if (visualizer.currentIndex < candidates.size) {
      val rect = candidates[visualizer.currentIndex]
      infoLabel.text =
          "<html>" +
              "Rectangle ${visualizer.currentIndex + 1}/${candidates.size}<br>" +
              "Corners: [${rect.i},${rect.j}] = ${rect.c1} to ${rect.c2}<br>" +
              "Bounds: (${rect.left},${rect.top}) to (${rect.right},${rect.bottom})<br>" +
              "Area: ${rect.area}<br>" +
              "Has intersection: ${rect.hasIntersection}<br>" +
              "Center inside: ${rect.centerInside}<br>" +
              "Valid: <b>${rect.isValid}</b>" +
              "</html>"
    } else {
      infoLabel.text = "No candidates"
    }
    visualizer.repaint()
  }

  val prevButton = JButton("Previous")
  prevButton.addActionListener {
    if (visualizer.currentIndex > 0) {
      visualizer.currentIndex--
      updateInfo()
    }
  }

  val nextButton = JButton("Next")
  nextButton.addActionListener {
    if (visualizer.currentIndex < candidates.size - 1) {
      visualizer.currentIndex++
      updateInfo()
    }
  }

  val validOnlyCheckbox = JCheckBox("Valid only", false)
  validOnlyCheckbox.addActionListener {
    // TODO: filter candidates
  }

  val controlPanel = JPanel()
  controlPanel.add(prevButton)
  controlPanel.add(nextButton)
  controlPanel.add(validOnlyCheckbox)

  frame.layout = BorderLayout()
  frame.add(visualizer, BorderLayout.CENTER)
  frame.add(infoLabel, BorderLayout.NORTH)
  frame.add(controlPanel, BorderLayout.SOUTH)

  frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.isVisible = true

  updateInfo()
}

fun main() {
  val input = """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

  debugInteractive(input)
}
