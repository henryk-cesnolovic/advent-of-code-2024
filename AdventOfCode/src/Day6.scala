import scala.annotation.tailrec
import scala.util.{Try, Success}
import java.util.concurrent.atomic.AtomicLong
def prepareInput =
  scala.io.Source.fromResource("day6/input.txt").getLines.toSeq

@main def day6Solution1 =
  val input = prepareInput

  case class Guard(symbol: Char, positionI: Int, positionJ: Int)

  val guardSymbols = List('^', '>', '<', 'v')
  @tailrec
  def findGuard(i: Int = 0, j: Int = 0): Guard =
    if (j == input(i).size)
      findGuard(i + 1, 0)
    else if (guardSymbols.contains(input(i)(j)))
      Guard(input(i)(j), i, j)
    else
      findGuard(i, j + 1)

  @tailrec
  def moveAndCount(guard: Guard, steps: Set[(Int, Int)] = Set()): Int =
    if (List(-1, input.size).contains(guard.positionI) || List(-1, input(0).size).contains(guard.positionJ)) steps.size
    else
      val currentPosition = (guard.positionI, guard.positionJ)
      input(guard.positionI)(guard.positionJ) match
        case '#' =>
          guard.symbol match
            case '^' => moveAndCount(Guard('>', guard.positionI + 1, guard.positionJ + 1), steps)
            case '>' => moveAndCount(Guard('v', guard.positionI + 1, guard.positionJ - 1), steps)
            case 'v' => moveAndCount(Guard('<', guard.positionI - 1, guard.positionJ - 1), steps)
            case '<' => moveAndCount(Guard('^', guard.positionI - 1, guard.positionJ + 1), steps)
        case _ =>
          guard.symbol match
            case '^' => moveAndCount(Guard(guard.symbol, guard.positionI - 1, guard.positionJ), steps + currentPosition)
            case '>' => moveAndCount(Guard(guard.symbol, guard.positionI, guard.positionJ + 1), steps + currentPosition)
            case 'v' => moveAndCount(Guard(guard.symbol, guard.positionI + 1, guard.positionJ), steps + currentPosition)
            case '<' => moveAndCount(Guard(guard.symbol, guard.positionI, guard.positionJ - 1), steps + currentPosition)

  println(moveAndCount(findGuard()))
