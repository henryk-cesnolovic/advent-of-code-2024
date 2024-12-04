import scala.annotation.tailrec
import scala.util.{Try, Success}
def prepareInput =
  scala.io.Source.fromResource("day4/input.txt").getLines.toSeq

@main def day4Solution1 =
  val input = prepareInput

  def checkIfXMAS(value: Try[String]): Int =
    value match
      case Success("XMAS") => 1
      case _               => 0

  @tailrec
  def iterate(i: Int = 0, j: Int = 0, count: Int = 0): Int =
    if (i == input.size) count
    else
      val stageCount =
        checkIfXMAS(Try(s"${input(i)(j)}${input(i + 1)(j)}${input(i + 2)(j)}${input(i + 3)(j)}")) +
          checkIfXMAS(Try(s"${input(i)(j)}${input(i + 1)(j + 1)}${input(i + 2)(j + 2)}${input(i + 3)(j + 3)}")) +
          checkIfXMAS(Try(s"${input(i)(j)}${input(i)(j + 1)}${input(i)(j + 2)}${input(i)(j + 3)}")) +
          checkIfXMAS(Try(s"${input(i)(j)}${input(i - 1)(j + 1)}${input(i - 2)(j + 2)}${input(i - 3)(j + 3)}")) +
          checkIfXMAS(Try(s"${input(i)(j)}${input(i - 1)(j)}${input(i - 2)(j)}${input(i - 3)(j)}")) +
          checkIfXMAS(Try(s"${input(i)(j)}${input(i - 1)(j - 1)}${input(i - 2)(j - 2)}${input(i - 3)(j - 3)}")) +
          checkIfXMAS(Try(s"${input(i)(j)}${input(i)(j - 1)}${input(i)(j - 2)}${input(i)(j - 3)}")) +
          checkIfXMAS(Try(s"${input(i)(j)}${input(i + 1)(j - 1)}${input(i + 2)(j - 2)}${input(i + 3)(j - 3)}"))
      val newi = if (j == input(i).size - 1) i + 1 else i
      val newj = if (newi == i) j + 1 else 0
      iterate(newi, newj, count + stageCount)

  println(iterate())

@main def day4Solution2 =
  val input = prepareInput

  def checkIfXMAS(value: Try[String]): Boolean =
    value match
      case Success("MAS") => true
      case Success("SAM") => true
      case _              => false

  @tailrec
  def iterate(i: Int = 0, j: Int = 0, count: Int = 0): Int =
    if (i == input.size) count
    else
      val found =
        checkIfXMAS(Try(s"${input(i)(j)}${input(i + 1)(j + 1)}${input(i + 2)(j + 2)}")) &&
          checkIfXMAS(Try(s"${input(i)(j + 2)}${input(i + 1)(j + 1)}${input(i + 2)(j)}"))
      val newi = if (j == input(i).size - 1) i + 1 else i
      val newj = if (newi == i) j + 1 else 0
      iterate(newi, newj, if (found) count + 1 else count)

  println(iterate())
