object Day11 {
  def main(args: Array[String]): Unit = {
    println(day11Part2("ne,nw,se,nw,ne,s,s,s,sw,ne,sw,sw"))
  }

  def day11Part1(s: String): Int = {
    val pos = s.split(",").map(s => Step.withName(s.trim)).foldLeft((0, 0))((p, m) => Step.move(m, p))

    getSteps(pos)
  }

  def day11Part2(s: String): Int = {
    s.split(",").map(s => Step.withName(s.trim)).scanLeft((0, 0))((p, m) => {
      Step.move(m, p)
    }).map(getSteps).max
  }

  def getSteps(pos: (Int, Int)): Int = List(Math.abs(pos._1), Math.abs(pos._2), Math.abs(pos._2 - pos._1)).max

  object Step extends Enumeration {
    type Step = Value
    val n, ne, se, s, sw, nw = Value
    // Tilted y axis
    def move(v: Value, p: (Int, Int)): (Int, Int) = v match {
      case `n` => (p._1, p._2 + 1)
      case `ne` => (p._1 + 1, p._2 + 1)
      case `se` => (p._1 + 1, p._2)
      case `s` => (p._1, p._2 - 1)
      case `sw` => (p._1 - 1, p._2 - 1)
      case `nw` => (p._1 - 1, p._2)
    }
  }
}
