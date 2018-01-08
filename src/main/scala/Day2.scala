object Day2 {
  def main(args: Array[String]): Unit = {
    //    println(day2Part2())
  }

  def day2Part1(s: String): Int = {
    s.split("\n").foldLeft(0)((acc, l) => {
      val values = l.split("\t").map(_.toInt)
      acc + (values.max - values.min)
    })
  }

  def day2Part2(s: String): Int = {
    s.split("\n").foldLeft(0)((acc, l) => {
      val values = l.split("\t").map(_.toInt).sorted(Ordering[Int].reverse)
      acc + getEvenlyDivisible(values)
    })
  }

  def getEvenlyDivisible(sortedInts: Array[Int]): Int = {
    val l = sortedInts.length
    for (i <- sortedInts.indices) {
      for (n <- i + 1 until l) {
        if (sortedInts(i) % sortedInts(n) == 0) return sortedInts(i) / sortedInts(n)
      }
    }

    0
  }
}
