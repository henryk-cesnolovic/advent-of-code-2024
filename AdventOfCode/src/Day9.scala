import scala.annotation.tailrec
import scala.util.{Try, Success}
import scala.collection.mutable

def prepareInput =
  scala.io.Source.fromResource("day9/input.txt").getLines().toSeq.head

@main def day9Solution1 =
  val input = prepareInput

  val repr = scala.collection.mutable.ArrayBuffer.empty[Int | String]
  @tailrec
  def representation(i: Int = 0): Unit =
    if (i == input.size) ()
    else {
      val symbol = if (i % 2 == 0) (i / 2) else "."
      (0 to (input(i) - 49)).foreach(_ => repr += symbol)
      representation(i + 1)
    }

  representation()

  // for better perf
  val result = scala.collection.mutable.ArrayBuffer.empty[Int]
  @tailrec
  def move(i: Int = 0, j: Int = repr.size - 1): Unit =
    if (j <= i)
      repr(i) match
        case value: Int  => result += value
        case dot: String => ()
    else if (i < j)
      repr(j) match
        case dot: String => move(i, j - 1)
        case valueJ: Int =>
          repr(i) match
            case dot: String =>
              result += valueJ
              move(i + 1, j - 1)
            case valueI: Int =>
              result += valueI
              move(i + 1, j)

  move()
  println(result.zipWithIndex.map { case (value, index) => value.toLong * index }.reduce(_ + _))

@main def day9Solution2 =
  val input = prepareInput

  // val repr = scala.collection.mutable.ArrayBuffer.empty[Array[Int | String]
  // @tailrec
  // def representation(i: Int = 0): Unit =
  //   if (i == input.size) ()
  //   else {
  //     val symbol = if (i % 2 == 0) (i / 2) else "."
  //     (0 to (input(i) - 49)).foreach(_ => repr += symbol)
  //     representation(i + 1)
  //   }
  //
  // representation()
  //
  // // for better perf
  // val result = scala.collection.mutable.ArrayBuffer.empty[Int]
  // @tailrec
  // def move(i: Int = 0, j: Int = repr.size - 1): Unit =
  //   if (j <= i)
  //     repr(i) match
  //       case value: Int  => result += value
  //       case dot: String => ()
  //   else if (i < j)
  //     repr(j) match
  //       case dot: String => move(i, j - 1)
  //       case valueJ: Int =>
  //         repr(i) match
  //           case dot: String =>
  //             result += valueJ
  //             move(i + 1, j - 1)
  //           case valueI: Int =>
  //             result += valueI
  //             move(i + 1, j)
  //
  // move()
  // println(result.zipWithIndex.map { case (value, index) => value.toLong * index }.reduce(_ + _))
