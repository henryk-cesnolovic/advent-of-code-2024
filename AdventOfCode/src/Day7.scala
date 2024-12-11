import scala.annotation.tailrec

def prepareInput =
  scala.io.Source.fromResource("day7/input.txt").getLines.toSeq

@main def day7Solution1 =
  val input = prepareInput

  def it(expected: Long, acc: Long, leftNumbers: List[Long]): Boolean =
    leftNumbers match {
      case head :: Nil =>
        (head + acc) == expected || (head * acc) == expected
      case head :: tail =>
        it(expected, acc + head, tail) || it(expected, acc * head, tail)
    }

  val result = input
    .map { case line =>
      val Array(value, numbersString) = line.split(":")
      (value.trim.toLong, numbersString.trim.split(" ").map(_.toLong).toList)
    }
    .filter { case (value, numbers) =>
      it(value, numbers.head, numbers.drop(1))
    }
    .foldLeft(0L)(_ + _._1)
  println(result)

@main def day7Solution2 =
  val input = prepareInput

  def it(expected: Long, acc: Long, leftNumbers: List[Long]): Boolean =
    leftNumbers match {
      case head :: Nil =>
        (head + acc) == expected || (head * acc) == expected || s"$acc$head".toLong == expected
      case head :: tail =>
        it(expected, acc + head, tail) || it(expected, acc * head, tail) || it(expected, s"$acc$head".toLong, tail)
    }

  val result = input
    .map { case line =>
      val Array(value, numbersString) = line.split(":")
      (value.trim.toLong, numbersString.trim.split(" ").map(_.toLong).toList)
    }
    .filter { case (value, numbers) =>
      it(value, numbers.head, numbers.drop(1))
    }
    .foldLeft(0L)(_ + _._1)
  println(result)
