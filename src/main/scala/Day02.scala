import scala.io.Source

object Day02 extends App {
  case class Movement(direction: String, steps: Int)
  case class NormalPosition(x: Int, y: Int)
  case class AimedPosition(x: Int, y: Int, a: Int)

  val source = Source.fromFile("input/day02.input")
  val movements = source.getLines().map(inputToMovement).toList

  val calculatedPosition = movements.foldLeft(NormalPosition(0, 0)){ case (p, m) => calculateNextNormalPosition(p, m) }

  println(s"Position: ${calculatedPosition}")
  println(s"Result: ${calculatedPosition.x * calculatedPosition.y}")

  val aimedCalculatedPosition = movements.foldLeft(AimedPosition(0, 0, 0)){ case (p, m) => calculateNextAimedPosition(p, m) }
  println(s"Aimed Position: ${aimedCalculatedPosition}")
  println(s"Aimed Result: ${aimedCalculatedPosition.x * aimedCalculatedPosition.y}")

  def calculateNextNormalPosition(position: NormalPosition, movement: Movement): NormalPosition = {
    movement match {
      case Movement("down", num) => position.copy(y = position.y + num)
      case Movement("up", num) => position.copy(y = position.y - num)
      case Movement("forward", num) => position.copy(x = position.x + num)
    }
  }
  def calculateNextAimedPosition(position: AimedPosition, movement: Movement): AimedPosition = {
    movement match {
      case Movement("down", num) => position.copy(a =  position.a + num)
      case Movement("up", num) => position.copy(a = position.a - num)
      case Movement("forward", num) => position.copy(x = position.x + num, y = position.y + (position.a * num))
    }
  }

  def inputToMovement(input: String) = {
    input.split(' ').toList match {
      case dir :: num :: Nil => Movement(dir, num.toInt)
    }
  }
}


