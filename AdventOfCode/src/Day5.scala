import scala.annotation.tailrec
import scala.util.{Try, Success}
def prepareInput =
  val lines = scala.io.Source.fromResource("day5/input.txt").getLines.toSeq
  val splitIndex = lines.zipWithIndex.find { case (line, index) => line == "" }.get._2
  val (rules, pageNumbers) = lines.splitAt(splitIndex)
  (rules, pageNumbers.drop(1))

@main def day5Solution1 =
  val (rules, pageNumbers) = prepareInput
  val pattern = """(\d+)\|(\d+)""".r

  val rulesPrepared = rules.foldLeft(Map[Int, Set[Int]]()) { case (map, rule) =>
    val pattern(left, right) = rule
    val set = map.get(left.toInt).map(_ + right.toInt).getOrElse(Set(right.toInt))
    map + (left.toInt -> set)
  }

  val result = pageNumbers.foldLeft(0) { case (acc, pageNumbersLine) =>
    val numbers = pageNumbersLine.split(",").map(_.toInt)
    val numbersPairs = numbers.sliding(2).toSeq
    val isCorrect = numbersPairs.filterNot { case Array(left, right) =>
      rulesPrepared.get(left) match {
        case None      => false
        case Some(set) => set.contains(right)
      }
    }.size == 0

    if (isCorrect)
      acc + numbers(Math.floor(numbers.size / 2).toInt)
    else
      acc
  }
  println(result)

@main def day5Solution2 =
  val (rules, pageNumbers) = prepareInput
  val pattern = """(\d+)\|(\d+)""".r

  val rulesPrepared = rules.foldLeft(Map[Int, Set[Int]]()) { case (map, rule) =>
    val pattern(left, right) = rule
    val set = map.get(left.toInt).map(_ + right.toInt).getOrElse(Set(right.toInt))
    map + (left.toInt -> set)
  }

  val result = pageNumbers.foldLeft(0) { case (acc, pageNumbersLine) =>
    val numbers = pageNumbersLine.split(",").map(_.toInt).toList
    val numbersPairs = numbers.sliding(2).toSeq
    val isCorrect = numbersPairs.filterNot { case List(left, right) =>
      rulesPrepared.get(left) match {
        case None      => false
        case Some(set) => set.contains(right)
      }
    }.size == 0

    @tailrec
    def swapNumbers(numbersAcc: List[Int], i: Int = 0, j: Int = 1): List[Int] =
      if (j < numbersAcc.size)
        val iValue = numbersAcc(i)
        val jValue = numbersAcc(j)
        rulesPrepared.get(jValue) match
          case Some(set) if set.contains(iValue) =>
            swapNumbers(numbersAcc.updated(i, jValue).updated(j, iValue), i, i + 1)
          case _ => swapNumbers(numbersAcc, i, j + 1)
      else if (i < numbersAcc.size - 1)
        swapNumbers(numbersAcc, i + 1, i + 2)
      else numbersAcc

    if (isCorrect) acc
    else
      val fixed = swapNumbers(numbers)
      acc + fixed(Math.floor(fixed.size / 2).toInt)
  }
  println(result)
