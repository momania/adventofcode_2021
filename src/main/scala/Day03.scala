import scala.annotation.tailrec
import scala.io.Source

object Day03 extends App {

  val source = Source.fromFile("input/day03.input")
  val lines = source.getLines().toList.map(_.map(_.asDigit).toList)

  val transposed = lines.transpose

  val mostCommon = transposed.map { bits =>
    val ones = bits.count(_ == 1)
    if (ones > bits.length / 2) 1 else 0
  }

  val leastCommon = mostCommon.map(_ ^ 1)

  println(s"Most common: ${mostCommon}")
  println(s"Least common: ${leastCommon}")

  val gammaRate = Integer.parseInt(mostCommon.mkString, 2)
  val epsilonRate = Integer.parseInt(leastCommon.mkString, 2)
  println(s"Gamma: ${gammaRate} - Epsilon: ${epsilonRate}")

  val power = gammaRate * epsilonRate
  println(s"Power consumption: $power")

  @tailrec def reduceNumbers(input: List[List[Int]], index: Int = 0, compare: (Int, Int) => Int): List[Int] = {
    val t = input.transpose.drop(index).head
    val sigs = t.count(_ == 1)
    val significant = compare(sigs, t.length)
    val filteredList = input.filter(_.drop(index).head == significant)
    if (filteredList.length > 1) {
      reduceNumbers(filteredList, index + 1, compare)
    } else {
      filteredList.head
    }
  }

  val oxygenRating = reduceNumbers(lines, compare = {case (s, t) => if (s >= t / 2.0) 1 else 0})
  val scrubberRating = reduceNumbers(lines, compare = {case (s, t) => if (s < t / 2.0) 1 else 0})

  val oxygen = Integer.parseInt(oxygenRating.mkString, 2)
  val scrubber = Integer.parseInt(scrubberRating.mkString, 2)
  println(s"Oxygen: ${oxygen} - Scrubber: ${scrubber}")

  val lifeSupport = oxygen * scrubber
  println(s"Life support: $lifeSupport")

}

