object Day7 {
  def main(args: Array[String]): Unit = {
    //    println(day7Part2())
  }

  case class Program(name: String, weight: Int, children: List[Program], totalWeight: Int)

  def parseProgramData(s: String): List[(String, Int, List[String])] = s.split("\n").map(
    p => {
      val l = p.split("->")
      val pData = """\w+""".r.findAllIn(l(0)).toList

      // Assuming there is always a head and a tail in the data structure
      (
        pData.head.replace(" ", ""),
        pData(1).toInt,
        l.lift(1).map(_.split(", ").toList.map(_.replace(" ", ""))).getOrElse(Nil)
      )
    }
  ).toList

  def getProgram(parsed: List[(String, Int, List[String])], p: (String, Int, List[String])): Program = {
    val tempP = Program(p._1, p._2, p._3.map(p => getProgram(parsed, parsed.find(_._1 == p).get)), 0)

    tempP.copy(totalWeight = getProgramTotalWeight(tempP))
  }

  def getProgramTotalWeight(p: Program): Int = {
    def goChildren(acc: Int, c: List[Program]): Int = {
      c match {
        case Nil => acc
        case _ => c.head.children match {
          case Nil => goChildren(acc + c.head.weight, c.tail)
          case _ => goChildren(acc + c.head.weight, c.head.children ++ c.tail)
        }
      }
    }

    p.weight + goChildren(0, p.children)
  }

  def getCorrectWeight(l: List[Program]): Int = {
    // One bad program possible
    l.groupBy(_.totalWeight).filter(_._2.length > 1).head._2.head.totalWeight
  }

  def day7Part2(s: String): Int = {
    def goRoot(correctWeight: Int, l: List[Program]): Int = {
      if (l.length == 1) l.head.weight - (l.head.totalWeight - correctWeight)
      else if (l.head.totalWeight == correctWeight) {
        goRoot(correctWeight, l.tail)
      } else {
        goRoot(getCorrectWeight(l.head.children), l.head.children)
      }
    }

    val parsed = parseProgramData(s)
    val data = parsed.map(p => getProgram(parsed, p))
    val root = data.find(_.name == "svugo") // Use part 1 to get this

    goRoot(getCorrectWeight(root.get.children), root.get.children)
  }

  def day7Part1(s: String): String = {
    val split = s.split("\n")

    val mainChildren = split.flatMap(p => {
      p.split("->").lift(1).foldLeft(List[String]())((acc, children) => {
        acc ++ children.split(", ").map(_.replace(" ", ""))
      })
    })

    val mainPrograms = split.foldLeft(List[String]())((acc, p) => {
      val prog = p.split("->")
      if (prog.length > 1) {
        acc ++ List("""[a-z]+""".r.findFirstIn(prog(0)).get.replace(" ", ""))
      } else acc
    })

    mainPrograms.diff(mainChildren).head
  }
}
