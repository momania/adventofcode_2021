import scala.io.Source

object Day07 extends App {

  val source = Source.fromFile("input/day07.input")
  val lines = source.getLines().flatMap(_.split(',').map(_.toInt)).toList

  val max = lines.max
  val minFuelOne = (1 to max).map { pos => lines.foldLeft(0){case (s, p) => s + math.abs(pos - p)}}
  println(s"Min fuel 1: ${minFuelOne.min}")

  val minFuelTwo = (1 to max).map { pos =>
    lines.foldLeft(0){
      case (s, p) =>
        val d = math.abs(pos - p)
        val x = (d  * d / 2.0) + (d / 2.0)
        s + x.toInt
    }
  }
  println(s"Min fuel 2: ${minFuelTwo.min}")
}

