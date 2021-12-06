import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App {

  val source = Source.fromFile("input/day06.input.test")
  val initialFish = source.getLines().flatMap(_.split(',').map(_.toInt)).toList

  val fish = breadFish(initialFish, 80)
  println(s"Total fish ${fish.length}")

  @tailrec def breadFish(fish: List[Int], steps: Int): List[Int] = {
    println(s"Step: $steps")
    val newFish = List.fill(fish.count(_ == 0))(8)
    val bread = fish.collect {
      case 0 => 6
      case x => x - 1
    }
    val totalFish = bread ++ newFish
    if (steps > 1) {
      breadFish(totalFish, steps - 1)
    } else {
      totalFish
    }

  }
}

