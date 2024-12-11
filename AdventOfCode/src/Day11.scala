import scala.annotation.tailrec
import scala.util.{Try, Success}

def prepareInput =
  scala.io.Source.fromResource("day11/test_input.txt").getLines().toSeq.head

@main def day11Solution1 =
  val input = prepareInput

  val initStones = input.split(" ")

  @tailrec
  def it(i: Int, stonesAcc: Array[String]): Array[String] =
    if (i == 25) stonesAcc
    else
      val prep = scala.collection.mutable.ArrayBuffer.empty[String]
      stonesAcc.foreach { stone =>
        stone match {
          case "0" => prep += "1"
          case value if value.size % 2 == 0 =>
            value.grouped(value.size / 2).foreach(s => prep += s.toLong.toString)
          case value => prep += (value.toLong * 2024).toString
        }
      }
      it(i + 1, prep.toArray)

  println(it(0, initStones).size)

@main def day11Solution2 =
  val input = prepareInput

  val initStones = input.split(" ").map(_.toLong)

  import scala.collection.mutable.Map
  val cache: Map[Long, Map[Int, Long]] = Map.empty[Long, Map[Int, Long]]

  def it(stone: Long): Long =
    def it(i: Int, stone: Long, accSum: Long = 0): Long =
      val newSum =
        if (i == 2) accSum + 1
        else
          stone match {
            case 0 =>
              cache.get(1L) match {
                case Some(cm) =>
                  cm.get(75 - i) match {
                    case Some(v: Long) =>
                      accSum + v
                    case None =>
                      val tmp = it(i + 1, 1L)
                      cache(1L)(i) = accSum + tmp
                      accSum + tmp
                  }
                case None =>
                  val tmp = it(i + 1, 1L)
                  cache(1L) = Map(i -> (tmp + accSum))
                  accSum + tmp
              }
            case value if value.toString.size % 2 == 0 =>
              val strRepr = value.toString
              val Seq(left, right) = strRepr.grouped(strRepr.size / 2).toSeq.map(_.toLong)

              cache.get(left) match {
                case Some(cm) =>
                  cm.get(75 - i) match {
                    case Some(v: Long) =>
                      accSum + v
                    case None =>
                      val tmp = it(i + 1, left)
                      cache(left)(i) = tmp + accSum
                      accSum + tmp
                  }
                case None =>
                  val tmp = it(i + 1, left)
                  cache(left) = Map(i -> (tmp + accSum))
                  accSum + tmp
              }

              cache.get(right) match {
                case Some(cm) =>
                  cm.get(75 - i) match {
                    case Some(v: Long) =>
                      accSum + v
                    case None =>
                      val tmp = it(i + 1, right)
                      cache(right)(i) = tmp + accSum
                      accSum + tmp
                  }
                case None =>
                  val tmp = it(i + 1, right)
                  cache(right) = Map(i -> (tmp + accSum))
                  accSum + tmp
              }

            case value =>
              cache.get(value) match {
                case Some(cm) =>
                  cm.get(75 - i) match {
                    case Some(v: Long) =>
                      accSum + v
                    case None =>
                      val tmp = it(i + 1, value * 2024)
                      cache(value)(i) = tmp + accSum
                      accSum + tmp
                  }
                case None =>
                  val tmp = it(i + 1, value * 2024)
                  cache(1L) = Map(i -> (tmp + accSum))
                  accSum + tmp
              }

          }
      // cache
      newSum
    it(0, stone)

  println(initStones.take(1).foldLeft(0L) { case (acc, s) =>
    acc + it(s)
  })

  println(s"cache $cache")
