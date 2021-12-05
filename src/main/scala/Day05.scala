import scala.io.Source

object Day05 extends App {

  val source = Source.fromFile("input/day05.input")
  val lines = source.getLines().toList

  val InputParser = """(\d+),(\d+).->.(\d+),(\d+)""".r

  case class GridItem(x: Int, y: Int, lines: Int)
  case class Line(from: (Int, Int), to: (Int, Int))

  val inputLines = lines.map(inputToLine)
  val nonDiagonals = inputLines.filterNot(isDiagonal)
  println(s"Non diagonals:\n${nonDiagonals.mkString("\n")}")

  val coordinates = nonDiagonals.map(lineToCoordinates)
  val numberOverlapping = coordinates.flatten.groupBy(identity).count(_._2.length > 1)
  println(s"Overlapping: $numberOverlapping")

  val allCoordinates = inputLines.map(lineToCoordinates)
  val allNumberOverlapping = allCoordinates.flatten.groupBy(identity).count(_._2.length > 1)
  println(s"All Overlapping: $allNumberOverlapping")

  def lineToCoordinates(line: Line): List[(Int, Int)] = {
    val xSign = (line.to._1 - line.from._1).sign
    val ySign = (line.to._2 - line.from._2).sign
    lazy val xRange = Range.inclusive(line.from._1, line.to._1, xSign)
    lazy val yRange = Range.inclusive(line.from._2, line.to._2, ySign)
    val diffX = math.abs(line.from._1 - line.to._1)
    val diffY = math.abs(line.from._2 - line.to._2)
    val coordinates = if (ySign == 0 || xSign == 0) {
      if (xSign == 0) {
        for (y <- yRange) yield (line.from._1, y)
      } else {
        for (x <- xRange) yield (x, line.from._2)
      }
    } else if (diffX == diffY) {
      for ((x, y) <- xRange zip yRange) yield (x, y)
    } else {
      println(s"Invalid line! $line")
      throw new Exception("Invalid Line")
    }
    println(s"Coordinates for line $line ($diffX, $diffY): $coordinates")
    coordinates.toList
  }

  def inputToLine(input: String): Line = {
    input match {
      case InputParser(x1, y1, x2, y2) =>
        Line((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
    }
  }

  def isDiagonal(line: Line): Boolean = {
    line.from._1 != line.to._1 && line.from._2 != line.to._2
  }
}

