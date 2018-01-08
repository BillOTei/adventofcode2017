object Day1 {
  def main(args: Array[String]): Unit = {
    //    println(day1Part2())
  }

  def day1Part1(s: String): Int = {
    s.zipWithIndex.foldLeft(0)((acc, t) => {
      if ((s.length - 1 == t._2 && s(0) == t._1) || s(t._2 + 1) == t._1) acc + t._1.asDigit
      else acc
    })
  }

  def day1Part2(s: String): Int = {
    // Assuming s length is always even
    val l = s.length
    val s1 = s.substring(0, l / 2)
    val s2 = s.substring(l / 2)

    s1.zipWithIndex.foldLeft(0)((acc, t) => {
      if (s2(t._2) == t._1) acc + t._1.asDigit * 2
      else acc
    })
  }
}
