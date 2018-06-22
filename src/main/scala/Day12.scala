import scala.collection.mutable.ArrayBuffer

object Day12 {
  def main(args: Array[String]): Unit = {
    println(day12Part1("0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"))
  }

  /**
    * Not very nicely done, quite imperative and involving buffers
    * @param s the input
    * @return
    */
  def day12Part1(s: String): Int = {
    // Keep raw data as array for faster index look up
    val rawData = s.split("\n").map(p => {
      val pData = p.split(" <-> ")
      // Zip with idx like
      (pData(0).toInt, pData(1).split(", ").toList.map(_.toInt))
    })

    // First buffer to store a reference of the children elements of the ones directly connected to 0
    val b = ArrayBuffer[Int]()

    // First and second results, directly connected to 0 elements and their children
    val r = rawData.foldLeft(Nil: List[Int])((acc, e) => {
      if (acc.isEmpty) List(0) ++ e._2
      else if (acc.contains(e._1)) {
        b ++= e._2

        acc
      } else if (e._2.intersect(acc).nonEmpty) acc ++ List(e._1)
      else acc
    })
    val r1 = (b filter(_ != 0) distinct).toList

    // Look up in the children result for all the children of children, could be recursive
    val b1 = r1.toBuffer
    val b2 = ArrayBuffer[Int]()
    while (b1.nonEmpty) {
      b1.foreach(v => {
        val toAddAndCheck = rawData.find(_._1 == v).get._2.filterNot(i => r.contains(i) || b2.contains(i))
        b1 ++= toAddAndCheck
        b2 ++= toAddAndCheck
        b1 -= v
      })
    }

    val r2 = b2.toList

    (r ++ r1 ++ r2 distinct).length
  }
}
