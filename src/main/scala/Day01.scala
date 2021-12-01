import scala.collection.immutable.Iterable
import scala.io.Source

object Day01 extends App {

  val source = Source.fromFile("input/day01.input")
  val lines = source.getLines().map(_.toInt).toList
  println(s"Increases: ${computeIncreases(lines)}")

  var sums = lines.sliding(3).map(_.sum).toList
  println(s"Increases 2: ${computeIncreases(sums)}")

  def computeIncreases(numbers: List[Int])= {
    numbers.sliding(2).count{case (h :: t :: Nil) => t > h}
  }
}

