object Day5 {
  def main(args: Array[String]): Unit = {
    //    println(day5Part2())
  }

  def day5Part2(s: String): Int = {
    val i = s.split("\n").map(_.toInt).toBuffer
    val l = i.length

    var r = 0
    var index = 0

    while (true) {
      if (index >= l || index < 0) return r
      r += 1
      val curIndex = index
      index += i(index)
      val incr = if (i(curIndex) >= 3) -1 else 1
      i(curIndex) += incr
    }

    0
  }

  def day5Part1(s: String): Int = {
    // Can be done with recursion, a bit more the functional way
    val i = s.split("\n").map(_.toInt).toBuffer
    val l = i.length

    var r = 0
    var index = 0

    while (true) {
      if (index >= l || index < 0) return r
      r += 1
      val curIndex = index
      index += i(index)
      i(curIndex) += 1
    }

    0
  }
}
