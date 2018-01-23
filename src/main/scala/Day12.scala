object Day12 {
  def main(args: Array[String]): Unit = {
    println(day12Part1("0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"))
  }

  def day12Part1(s: String): Int = {
    val rawData = s.split("\n").map(p => {
      val pData = p.split(" <-> ")
      // Data should be consistent
      (pData(0).toInt, pData(1).split(", ").toList.map(_.toInt))
    })


    0
  }
}
