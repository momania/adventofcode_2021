import scala.annotation.tailrec
import scala.io.Source

object Day04 extends App {

  val source = Source.fromFile("input/day04.input")
  val lines = source.getLines().toList

  case class BoardNumber(num: Int, marked: Boolean)
  case class Board(numbers: List[List[BoardNumber]])
  val numberToDraw = lines.head.split(',').map(_.toInt).toList
  val boards = collectBoards(lines.drop(2), List.empty, List.empty)

  val (winningNum, winningBoard) = drawNumbers(numberToDraw, boards)
  printResult(winningNum, winningBoard)

  val (lastWinningNum, lastWinningBoard) = drawNumberUntilAllDone(numberToDraw, boards)
  printResult(lastWinningNum, lastWinningBoard)

  def printResult(num: Int, board: Board): Unit = {
    println(s"Winning num: $num")
    println(s"WinningBoard:\n${board.numbers.mkString("\n")}")
    val sumUnmarked = board.numbers.flatten.filterNot(_.marked).map(_.num).sum
    println(s"Sum unmarked: $sumUnmarked")
    println(s"Score: ${sumUnmarked * num}")
  }
  @tailrec def drawNumbers(numbers: List[Int], boards: List[Board]): (Int, Board) = {
    numbers match {
      case n :: tail =>
        val updatedBoards = boards.map(b => markNumberOnBoard(b, n))
        updatedBoards.find(isWinningBoard) match {
          case Some(winning) => (n, winning)
          case None => drawNumbers(tail, updatedBoards)
        }
    }
  }

  @tailrec def drawNumberUntilAllDone(numbers: List[Int], boards: List[Board]): (Int, Board) = {
    numbers match {
      case n :: tail =>
        val openBoards = boards.filterNot(isWinningBoard)
        val updatedBoards = openBoards.map(b => markNumberOnBoard(b, n))
        if (updatedBoards.forall(isWinningBoard)) {
          (n, updatedBoards.head)
        } else {
          drawNumberUntilAllDone(tail, updatedBoards)
        }
    }
  }

  def markNumberOnBoard(board: Board, number: Int): Board = {
    val numbers = board.numbers.map(_.map(n => if(n.num == number) n.copy(marked = true) else n))
    Board(numbers)
  }
  def isWinningBoard(board: Board): Boolean = {
//    println(s"Winning?\n${board.numbers.mkString("\n")}")
    board.numbers.exists(_.forall(_.marked)) || board.numbers.transpose.exists(_.forall(_.marked))
  }
  @tailrec def collectBoards(input: List[String], currentBoard: List[List[BoardNumber]], collected: List[Board]): List[Board] = {
    input match {
      case h :: Nil =>
        collected :+ Board(currentBoard :+ extractBoardLine(h))
      case h :: tail if h.isEmpty =>
        collectBoards(tail, List.empty, collected :+ Board(currentBoard))
      case h :: tail =>
        collectBoards(tail, currentBoard :+ extractBoardLine(h), collected)
    }
  }

  def extractBoardLine(input: String): List[BoardNumber] = {
    input.dropWhile(_ == ' ').split("\\s+").map(c => BoardNumber(c.toInt, marked = false)).toList
  }
}

