object Day6 {
  def main(args: Array[String]): Unit = {
    //    println(day6Part2())
  }

  def day6Part2(l: List[Int]): Int = {
    def go(zippedL: List[(Int, Int)], maxI: Int, i: Int, toSpread: Int, rList: List[List[Int]]): Int = {
      if (toSpread == 0) {
        val max = zippedL.maxBy(_._1)
        // Back to square 0 or carry on
        val newI = if (max._2 < zippedL.size - 1) max._2 + 1 else 0

        // If found twice
        if (rList.contains(zippedL.unzip._1)) rList.length - rList.indexOf(zippedL.unzip._1)
        else go(zippedL, max._2, newI, max._1, rList ++ List(zippedL.unzip._1))

      } else {
        val decrL = getNewList(zippedL, -1, maxI)
        val incrL = getNewList(decrL, 1, i)
        val newI = if (i < zippedL.size - 1) i + 1 else 0

        go(incrL, maxI, newI, toSpread - 1, rList)
      }
    }

    val tList = l.zipWithIndex
    // Takes first max if several by default
    val max = tList.maxBy(_._1)

    // Could pass the size for optimization
    go(tList, max._2, max._2 + 1, max._1, Nil)
  }

  def day6Part1(l: List[Int]): Int = {
    def go(zippedL: List[(Int, Int)], maxI: Int, i: Int, toSpread: Int, rList: List[List[Int]]): Int = {
      if (toSpread == 0) {
        val max = zippedL.maxBy(_._1)
        // Back to square 0 or carry on
        val newI = if (max._2 < zippedL.size - 1) max._2 + 1 else 0

        // If found twice
        if (rList.contains(zippedL.unzip._1)) rList.length + 1
        else go(zippedL, max._2, newI, max._1, rList ++ List(zippedL.unzip._1))

      } else {
        val decrL = getNewList(zippedL, -1, maxI)
        val incrL = getNewList(decrL, 1, i)
        val newI = if (i < zippedL.size - 1) i + 1 else 0

        go(incrL, maxI, newI, toSpread - 1, rList)
      }
    }

    val tList = l.zipWithIndex
    // Takes first max if several by default
    val max = tList.maxBy(_._1)

    // Could pass the size for optimization
    go(tList, max._2, max._2 + 1, max._1, Nil)
  }

  private def getNewList(l: List[(Int, Int)], incr: Int, i: Int): List[(Int, Int)] = i match {
    case 0 => List((l(i)._1 + incr, i)) ++ l.slice(1, l.length)
    case _ if l.size - 1 == i => l.slice(0, i) ++ List((l(i)._1 + incr, i))
    case _ => l.slice(0, i) ++ List((l(i)._1 + incr, i)) ++ l.slice(i + 1, l.length)
  }
}
