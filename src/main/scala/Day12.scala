import scala.collection.mutable.ArrayBuffer

object Day12 {
  def main(args: Array[String]): Unit = {
    println(day12Part2("0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"))
  }



  /**
    * Not very nicely done, quite imperative and involving buffers
    * @param s the input
    * @return
    */
  def day12Part1(s: String): Int = group(rawData(s), 0).length

  def day12Part2(s: String): Int = {
    val data = rawData(s)

    val t = data.map(p => group(data, p._1))

    val test = findGroups(toMap(s.split("\n").toList))

    val test1 = t.groupBy(identity)

    test1.size
  }

  private def rawData(s: String): Array[(Int, List[Int])] = s.split("\n").map(p => {
    val pData = p.split(" <-> ")
    // Zip with idx like
    (pData(0).toInt, pData(1).split(", ").toList.map(_.toInt))
  })

  private def group(data: Array[(Int, List[Int])], leader: Int): List[Int] = {
    // First buffer to store a reference of the children elements of the ones directly connected to leader
    val b = ArrayBuffer[Int]()

    // First and second results, directly connected to leader elements and their children
    val start = List(leader) ++ data.find(_._1 == leader).get._2
    val r = data.foldLeft(start)((acc, e) => {
      if (acc.contains(e._1)) {
        b ++= e._2

        acc
      } else if (e._2.intersect(acc).nonEmpty) {
        b ++= e._2

        acc ++ List(e._1)
      } else acc
    })
    val r1 = (b filter(_ != leader) distinct).toList

    // Look up in the children result for all the children of children, could be recursive
    val b1 = r1.toBuffer
    val b2 = ArrayBuffer[Int]()
    while (b1.nonEmpty) {
      b1.foreach(v => {
        val toAddAndCheck = data.find(_._1 == v).get._2.filterNot(i => r.contains(i) || b2.contains(i))
        b1 ++= toAddAndCheck
        b2 ++= toAddAndCheck
        b1 -= v
      })
    }

    val r2 = b2.toList

    (r ++ r1 ++ r2).distinct.sorted
  }

  // Not mine
  def toMap(list: List[String]) : Map [Int, List[Int]] = {
    var map = scala.collection.mutable.Map[Int, List[Int]]()
    list.map(l => {
      val parts = l.split("<->")
      (parts(0).trim.toInt, parts(1).split(',').map(i => i.trim.toInt).toList)
    }).foreach(e => map += e._1 -> e._2)
    map.toMap
  }

  def connect(toWatch : List[Int], map : Map[Int, List[Int]], connected : Set[Int]) : Set[Int] = {
    toWatch match {
      case Nil => connected
      case x :: xs =>
        val stillToWatch = xs ::: map(x).filter(i => !toWatch.contains(i)).filter(i => !connected.contains(i))
        val allConnected = connected ++ map(x)
        connect(stillToWatch, map, allConnected)
    }
  }

  def findGroups(map : Map[Int, List[Int]]) : List[Set[Int]] = {
    def findGroups(pIdsNeedingAGroup : List[Int], pIdAlreadyGrouped : Set[Int], groups : List[Set[Int]]) : List[Set[Int]] = {
      pIdsNeedingAGroup match {
        case Nil => groups
        case x :: xs =>
          val ints = connect(List(x), map, Set())
          findGroups(pIdsNeedingAGroup.filter(i => !ints.contains(i)), pIdAlreadyGrouped ++ ints, ints :: groups)
      }
    }
    findGroups(map.keySet.toList, Set(), Nil)
  }
}
