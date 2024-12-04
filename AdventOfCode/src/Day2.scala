def prepareInput =
  scala.io.Source.fromResource("day2/input.txt")
    .getLines
    .map(_.split(" ").map(_.toInt).toList)
    .toSeq

@main def day2Solution1 = 
   val result = prepareInput
    .filter{row => 
      val grouped = row.sliding(2).toSeq
      val descending = grouped.filterNot{ pair => 
        val difference = pair(0) - pair(1)
        difference > 0 && difference < 4
      }
      val ascending = grouped.filterNot{ pair => 
        val difference = pair(0) - pair(1)
        difference > -4 && difference < 0
      }
      
      descending.size == 0 || ascending.size == 0
    }
    .size
   println(result)

@main def day2Solution2 = 
  def checkIfSafe(row: List[Int]) = 
    val grouped = row.sliding(2).toSeq
    val descending = grouped.filterNot{ pair => 
      val difference = pair(0) - pair(1)
      difference > 0 && difference < 4
    }
    val ascending = grouped.filterNot{ pair => 
      val difference = pair(0) - pair(1)
      difference > -4 && difference < 0
    }
    descending.size == 0 || ascending.size == 0

  val result = prepareInput
    .filter{row => 
      checkIfSafe(row) || (0 until row.size).exists{ it =>
        val newRow = row.take(it) ++ row.drop(it + 1)
        checkIfSafe(newRow)
      }
    }
   .size
  println(result)
