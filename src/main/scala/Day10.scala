object Day10 {
  def main(args: Array[String]): Unit = {
    println(day10Part2("102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216"))
  }

  def day10Part1(s: String): Int = {
    def go(pos: Int, skip: Int, l: List[Int], lengths: List[Int]): Int = lengths match {
      case Nil => l.head * l(1)
      case h :: tail => go((pos + h + skip) % l.length, skip + 1, getNewList(l, pos, h), tail)
    }

    go(0, 0, (0 to 255).toList, s.split(",").toList.map(_.toInt))
//    go(0, 0, (0 to 4).toList, List(3, 4, 1, 5))
  }

  def day10Part2(s: String): String = {
    def go(round: Int, pos: Int, skip: Int, l: List[Int], lengths: List[Int]): String = lengths match {
      case Nil if round >= 65 =>
        l.grouped(16).map(_.reduceLeft(_ ^ _)).foldLeft("")((acc, v) => {
          if (v.toHexString.length < 2) acc + "0" + v.toHexString else acc + v.toHexString
        })
      case Nil if round < 65 => go(round + 1, pos, skip, l, getASCIILengths(s))
      case h :: tail => go(round, (pos + h + skip) % l.length, skip + 1, getNewList(l, pos, h), tail)
    }

    go(1, 0, 0, (0 to 255).toList, getASCIILengths(s))
  }

  def getASCIILengths(s: String): List[Int] = s.toCharArray.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)

  def getNewList(l: List[Int], start: Int, length: Int): List[Int] = {
    val zipped = l.zipWithIndex
    val b = zipped.toBuffer
    val slice = getReversedSlice(zipped, start, length)
    for (i <- slice.indices) {
      // Flipping data in a mutable way :/
      val newPos = slice(slice.length - 1 - i)._2
      b(newPos) = slice(i)
    }

    b.toList.map(_._1)
  }

  def getReversedSlice(l: List[(Int, Int)], start: Int, length: Int): List[(Int, Int)] = {
    def go(slice: List[(Int, Int)], i: Int, len: Int): List[(Int, Int)] = {
      if (slice.length == length) slice
      else if (i < len) go(List(l(i)) ++ slice, i + 1, len)
      else go(List(l(i - len)) ++ slice, i + 1, len)
    }

    go(Nil, start % l.length, l.length)
  }
}
