import scala.io.Source

object Day02 extends App {
  sealed trait Direction
  object Direction {
    case object Up extends Direction
    case object Down extends Direction
    case object Forward extends Direction
    case object Backward extends Direction
  }
  case class Movement(direction: Direction, steps: Int)

  val source = Source.fromFile("input/day02.input")
  val movements = source.getLines().map(inputToMovement).toList

  val calculatedPosition = movements.foldLeft((0,0)) { case ((x, y), movement) =>
    movement match {
      case Movement(Direction.Down, num) => (x, y + num)
      case Movement(Direction.Up, num) => (x, y - num)
      case Movement(Direction.Forward, num) => (x + num, y)
      case Movement(Direction.Backward, num) => (x - num, y)
    }
  }

  println(s"Position: ${calculatedPosition}")
  var result = calculatedPosition._1 * calculatedPosition._2
  println(s"Result: ${result}")

  val aimedCalculatedPosition = movements.foldLeft((0,0,0)) { case ((x, y, a), movement) =>
    movement match {
      case Movement(Direction.Down, num) => (x, y, a + num)
      case Movement(Direction.Up, num) => (x, y, a - num)
      case Movement(Direction.Forward, num) => (x + num, y + (a * num), a)
      case Movement(Direction.Backward, num) => (x - num, y, a)
    }
  }
  println(s"Aimed Position: ${aimedCalculatedPosition}")
  var aimedResult = aimedCalculatedPosition._1 * aimedCalculatedPosition._2
  println(s"Aimed Result: ${aimedResult}")


  def inputToMovement(input: String) = {
    input.split(' ').toList match {
      case "forward" :: num :: Nil => Movement(Direction.Forward, num.toInt)
      case "backward" :: num :: Nil => Movement(Direction.Backward, num.toInt)
      case "up" :: num :: Nil => Movement(Direction.Up, num.toInt)
      case "down" :: num :: Nil => Movement(Direction.Down, num.toInt)
    }
  }
}


