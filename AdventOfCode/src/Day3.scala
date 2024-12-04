import scala.annotation.tailrec
def prepareInput =
  scala.io.Source.fromResource("day3/input.txt")
    .getLines
    .toSeq

@main def day3Solution1 = 
  val mulPattern = """mul\((\d{1,3}),(\d{1,3})\)""".r
  val lines = prepareInput
  val result = lines.foldLeft(0L){ case (acc, line) =>
    acc + 
    mulPattern
      .findAllIn(line)
      .map{ mulMatch =>
        val mulPattern(left, right) = mulMatch
        left.toLong * right.toLong
    }.reduce(_ + _)
  }
  println(result)


@main def day3Solution2 = 
  val mulPattern = """mul\((\d{1,3}),(\d{1,3})\)""".r
  val doPattern = """do\(\)""".r
  val dontPattern = """don't\(\)""".r
  val lines = prepareInput

  case class Result(sum: Long, enabled: Boolean)
  @tailrec
  def scanMemoryLine(lineToScan: String, enabled: Boolean = true, sum: Long = 0L): Result =
    if(enabled)
      val mulOpt = mulPattern.findFirstMatchIn(lineToScan)
      val dontOpt = dontPattern.findFirstMatchIn(lineToScan)
      mulOpt match {
        case None => Result(sum, enabled)
        case Some(mulValue) => dontOpt match {
          case Some(dontValue) if dontValue.start < mulValue.start =>
            scanMemoryLine(lineToScan.substring(dontValue.end), false, sum)
          case _ =>
            val mulPattern(left, right) = mulValue.matched
            scanMemoryLine(lineToScan.substring(mulValue.end), enabled, sum + (left.toLong * right.toLong))
        }
      }
    else
      val doOpt = doPattern.findFirstMatchIn(lineToScan)
      doOpt match {
        case None => Result(sum, enabled)
        case Some(doValue) => scanMemoryLine(lineToScan.substring(doValue.end), true, sum)
      }

  val result = lines.foldLeft((0L, true)){ case ((acc, enabled), line) =>
    val tempResult = scanMemoryLine(line, enabled)
    (acc + tempResult.sum, tempResult.enabled)
  }
  println(result._1)
