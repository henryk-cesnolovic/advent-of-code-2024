import scala.annotation.tailrec
import scala.util.{Try, Success}

def prepareInput =
  scala.io.Source.fromResource("day10/input.txt").getLines.toSeq.map(_.map(_.toInt - 48))

@main def day10Solution1 =
  val input = prepareInput

  @tailrec
  def collectStartinngPoints(i: Int = 0, j: Int = 0, accPoints: List[(Int, Int)] = List()): List[(Int, Int)] =
    if (i == prepareInput.size)
      accPoints
    else if (j == prepareInput(i).size)
      collectStartinngPoints(i + 1, 0, accPoints)
    else if (prepareInput(i)(j) == 0)
      collectStartinngPoints(i, j + 1, accPoints :+ (i, j))
    else
      collectStartinngPoints(i, j + 1, accPoints)

  val startingPoints = collectStartinngPoints()

  def countTrailHeads(i: Int, j: Int, tails: Set[(Int, Int)] = Set()): Set[(Int, Int)] =
    if (input(i)(j) == 9) tails ++ Set((i, j))
    else {
      val up = Try(input(i - 1)(j))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i - 1, j)
          else Set()
        }
        .getOrElse(Set())
      val right = Try(input(i)(j + 1))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i, j + 1)
          else Set()
        }
        .getOrElse(Set())
      val down = Try(input(i + 1)(j))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i + 1, j)
          else Set()
        }
        .getOrElse(Set())
      val left = Try(input(i)(j - 1))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i, j - 1)
          else Set()
        }
        .getOrElse(Set())
      up ++ right ++ down ++ left
    }

  println(startingPoints.map { case (i, j) => countTrailHeads(i, j) }.foldLeft(0)(_ + _.size))

@main def day10Solution2 =
  val input = prepareInput

  @tailrec
  def collectStartinngPoints(i: Int = 0, j: Int = 0, accPoints: List[(Int, Int)] = List()): List[(Int, Int)] =
    if (i == prepareInput.size)
      accPoints
    else if (j == prepareInput(i).size)
      collectStartinngPoints(i + 1, 0, accPoints)
    else if (prepareInput(i)(j) == 0)
      collectStartinngPoints(i, j + 1, accPoints :+ (i, j))
    else
      collectStartinngPoints(i, j + 1, accPoints)

  val startingPoints = collectStartinngPoints()

  def countTrailHeads(i: Int, j: Int, tails: Map[(Int, Int), Int] = Map()): Map[(Int, Int), Int] =
    if (input(i)(j) == 9) tails + tails.get((i, j)).map { value => (i, j) -> (value + 1) }.getOrElse((i, j) -> 1)
    else {
      val up = Try(input(i - 1)(j))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i - 1, j)
          else Map()
        }
        .getOrElse(Map())
      val right = Try(input(i)(j + 1))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i, j + 1)
          else Map()
        }
        .getOrElse(Map())
      val down = Try(input(i + 1)(j))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i + 1, j)
          else Map()
        }
        .getOrElse(Map())
      val left = Try(input(i)(j - 1))
        .map { value =>
          if (value == input(i)(j) + 1)
            countTrailHeads(i, j - 1)
          else Map()
        }
        .getOrElse(Map())

      val maps = Seq(up, right, down, left)
      maps.flatten
        .groupBy(_._1)
        .map { case (key, values) => key -> values.map(_._2).sum }
    }
  println(startingPoints.map { case (i, j) => countTrailHeads(i, j) }.foldLeft(0) { case (acc, m) => acc + m.values.reduce(_ + _) })
