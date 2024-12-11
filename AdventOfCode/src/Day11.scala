import scala.annotation.tailrec
import scala.util.{Try, Success}
import scala.annotation.init

def prepareInput =
  scala.io.Source.fromResource("day11/input.txt").getLines().toSeq.head

@main def day11Solution1 =
  val input = prepareInput

  val initStones = input.split(" ")

  def it(i: Int, nextStone: String): Long =
    if (i == 25) 1
    else
      nextStone match {
        case "0" => it(i + 1, "1")
        case value if value.size % 2 == 0 =>
          value.grouped(value.size / 2).map(_.toLong).map(s => it(i + 1, s.toString)).sum
        case value => it(i + 1, (value.toLong * 2024).toString)
      }

  println(initStones.foldLeft(0L) { case (acc, stone) => acc + it(0, stone) })

@main def day11Solution2 =
  val input = prepareInput

  val initStones = input.split(" ").map(_.toLong)

  import scala.collection.mutable.HashMap
  val cache: HashMap[(Long, Int), Long] = HashMap[(Long, Int), Long]()

  def it(i: Int, stone: Long): Long =
    if (i == 75) 1
    else
      cache.get((stone, i)) match
        case Some(cacheValue) => cacheValue
        case None =>
          val state =
            stone match {
              case 0 => it(i + 1, 1)
              case value if value.toString.size % 2 == 0 =>
                val valueStr = value.toString
                valueStr.grouped(valueStr.size / 2).map(_.toLong).map(s => it(i + 1, s)).sum
              case value => it(i + 1, (value * 2024))
            }
          cache((stone, i)) = state
          state

  println(initStones.foldLeft(0L) { case (acc, stone) => acc + it(0, stone) })
