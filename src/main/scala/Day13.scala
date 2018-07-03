object Day13 {
  def main(args: Array[String]): Unit = {
    println(day13Part2("0: 3\n1: 2\n4: 4\n6: 4"))
  }


  def day13Part1(s: String): Int = {
    val data = rawData(s)



    0
  }

  def day13Part2(s: String): Int = ???

  private def rawData(s: String): Array[(Int, Int)] = s.split("\n").map(p => {
    val pData = p.split(": ")
    // Zip with idx like
    (pData(0).toInt, pData(1).toInt)
  })
}
