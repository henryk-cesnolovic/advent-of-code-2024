def prepareInput =
  val input = scala.io.Source.fromResource("day1/input.txt")
  val locationsPattern = """(\d+)\W+(\d+)""".r
  input
    .getLines
    .map(row => {
      val locationsPattern(left, right) = row
      (left.toInt, right.toInt)
    })
    .toSeq

@main def day1Solution1() =
  val locations = prepareInput
  val left = locations.map(_._1).sorted
  val right = locations.map(_._2).sorted
  val result = left
    .zip(right)
    .foldLeft(0)((acc, locs) => acc + Math.abs(locs._1 - locs._2))
  println(result)

@main def day1Solution2() =
  val locations = prepareInput
  val left = locations.map(_._1)
  val right = locations.map(_._2).groupBy(number => number).mapValues(_.size)
  val result =
    left.foldLeft(0)((acc, number) => acc + right.getOrElse(number, 0) * number)
  println(result)
