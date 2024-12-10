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
//
// @main def day6Solution2 =
//   val input = prepareInput
//
//   case class Guard(symbol: Char, positionI: Int, positionJ: Int)
//
//   val guardSymbols = List('^', '>', '<', 'v')
//   @tailrec
//   def findGuard(i: Int = 0, j: Int = 0): Guard =
//     if (j == input(i).size)
//       findGuard(i + 1, 0)
//     else if (guardSymbols.contains(input(i)(j)))
//       Guard(input(i)(j), i, j)
//     else
//       findGuard(i, j + 1)
//
//   def checkIfItCanEscape(guard: Guard): Boolean =
//     if (List(0, input.size - 1).contains(guard.positionI) || List(0, input(0).size - 1).contains(guard.positionJ))
//       true
//
//     @tailrec
//     def checkIfItCanEscapeTail(guard: Guard, steps: Set[(Int, Int, Char)] = Set()): Boolean =
//       if (List(-1, input.size).contains(guard.positionI) || List(-1, input(0).size).contains(guard.positionJ))
//         true
//       else if (steps.contains((guard.positionI, guard.positionJ, guard.symbol)))
//         false
//       else
//         input(guard.positionI)(guard.positionJ) match
//           case '#' =>
//             guard.symbol match
//               case '^' => checkIfItCanEscapeTail(Guard('>', guard.positionI + 1, guard.positionJ - 1), steps)
//               case '>' => checkIfItCanEscapeTail(Guard('v', guard.positionI + 1, guard.positionJ - 1), steps)
//               case 'v' => checkIfItCanEscapeTail(Guard('<', guard.positionI - 1, guard.positionJ - 1), steps)
//               case '<' => checkIfItCanEscapeTail(Guard('^', guard.positionI - 1, guard.positionJ + 1), steps)
//           case _ =>
//             val state = (guard.positionI, guard.positionJ, guard.symbol)
//             guard.symbol match
//               case '^' => checkIfItCanEscapeTail(Guard(guard.symbol, guard.positionI - 1, guard.positionJ), steps + state)
//               case '>' => checkIfItCanEscapeTail(Guard(guard.symbol, guard.positionI, guard.positionJ + 1), steps + state)
//               case 'v' => checkIfItCanEscapeTail(Guard(guard.symbol, guard.positionI + 1, guard.positionJ), steps + state)
//               case '<' => checkIfItCanEscapeTail(Guard(guard.symbol, guard.positionI, guard.positionJ - 1), steps + state)
//
//     guard.symbol match
//       case '^' => checkIfItCanEscapeTail(Guard('>', guard.positionI, guard.positionJ + 1), Set((guard.positionI, guard.positionJ, guard.symbol)))
//       case '>' => checkIfItCanEscapeTail(Guard('v', guard.positionI + 1, guard.positionJ), Set((guard.positionI, guard.positionJ, guard.symbol)))
//       case 'v' => checkIfItCanEscapeTail(Guard('<', guard.positionI, guard.positionJ - 1), Set((guard.positionI, guard.positionJ, guard.symbol)))
//       case '<' => checkIfItCanEscapeTail(Guard('^', guard.positionI - 1, guard.positionJ), Set((guard.positionI, guard.positionJ, guard.symbol)))
//
//   @tailrec
//   def moveAndCount(guard: Guard, obstacles: Set[(Int, Int)] = Set()): Int =
//     if (List(-1, input.size).contains(guard.positionI) || List(-1, input(0).size).contains(guard.positionJ)) obstacles.size
//     else
//       val currentPosition = (guard.positionI, guard.positionJ)
//
//       input(guard.positionI)(guard.positionJ) match
//         case '#' =>
//           guard.symbol match
//             case '^' => moveAndCount(Guard('>', guard.positionI + 1, guard.positionJ + 1), obstacles)
//             case '>' => moveAndCount(Guard('v', guard.positionI + 1, guard.positionJ - 1), obstacles)
//             case 'v' => moveAndCount(Guard('<', guard.positionI - 1, guard.positionJ - 1), obstacles)
//             case '<' => moveAndCount(Guard('^', guard.positionI - 1, guard.positionJ + 1), obstacles)
//         case _ =>
//           val checkForLoop = !checkIfItCanEscape(Guard(guard.symbol, guard.positionI, guard.positionJ))
//           guard.symbol match
//             case '^' =>
//               val nextStep = (guard.positionI - 1, guard.positionJ)
//               moveAndCount(Guard(guard.symbol, nextStep._1, nextStep._2), if (checkForLoop) obstacles + nextStep else obstacles)
//             case '>' =>
//               val nextStep = (guard.positionI, guard.positionJ + 1)
//               moveAndCount(Guard(guard.symbol, nextStep._1, nextStep._2), if (checkForLoop) obstacles + nextStep else obstacles)
//             case 'v' =>
//               val nextStep = (guard.positionI + 1, guard.positionJ)
//               moveAndCount(Guard(guard.symbol, nextStep._1, nextStep._2), if (checkForLoop) obstacles + nextStep else obstacles)
//             case '<' =>
//               val nextStep = (guard.positionI, guard.positionJ - 1)
//               moveAndCount(Guard(guard.symbol, nextStep._1, nextStep._2), if (checkForLoop) obstacles + nextStep else obstacles)
//
//   println(moveAndCount(findGuard()))
