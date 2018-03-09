object Day12 {
  def main(args: Array[String]): Unit = {
    println(day12Part1("0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"))
  }

  case class Elmt(v: Int, children: List[Elmt])

  def day12Part1(s: String): Int = {
    val rawData = s.split("\n").map(p => {
      val pData = p.split(" <-> ")
      // Data should be consistent
      (pData(0).toInt, pData(1).split(", ").toList.map(_.toInt))
    })

    val t = rawData.map(t => (t._1, t._2.map(c => rawData.find(_._1 == c).get)))



    0
  }

  def getElmt(v: (Int, List[Int]), data: List[(Int, List[Int])]): Elmt = {
    Elmt(v._1, v._2.map(c => getElmt(data.find(_._1 == c).get, data)))
  }
}
