import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App {

  val source = Source.fromFile("input/day06.input")
  val input = source.getLines().flatMap(_.split(',').map(_.toInt)).toList

  val initialFish = for(i <- 0 to 8) yield input.count(_ == i).longValue
  println(s"Initial fish: $initialFish")
  val fish = breadFish(initialFish.toList, 256)
  println(s"Final fish: $fish")
  println(s"Total fish ${fish.sum}")

  @tailrec def breadFish(fish: List[Long], steps: Long): List[Long] = {
    val newFish = fish.head
    val rotated = fish.tail :+ newFish
    val bread = rotated.updated(6, rotated(6) + newFish)
    if (steps > 1) {
      breadFish(bread, steps - 1)
    } else {
      bread
    }
  }
}

