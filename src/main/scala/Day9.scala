object Day9 {
  def main(args: Array[String]): Unit = {
    println(day9Part1("{{{}}}"))
  }

  def day9Part1(s: String): Int = {
    // Groups and garbage always well formed
    def go(score: Int, groupWeight: Int, garbageOpened: Boolean, l: List[Char]): Int = l match {
      case Nil => score
      case h :: t => if (garbageOpened) h match {
        case '>' => go(score, groupWeight, garbageOpened = false, t)
        case '!' => go(score, groupWeight, garbageOpened, t.tail)
        case _ => go(score, groupWeight, garbageOpened, t)
      } else h match {
        case '<' => go(score, groupWeight, garbageOpened = true, t)
        case '{' => go(score + (groupWeight + 1), groupWeight + 1, garbageOpened, t)
        case '}' => go(score, groupWeight - 1, garbageOpened, t)
        case _ => go(score, groupWeight, garbageOpened, t)
      }
    }

    go(0, 0, garbageOpened = false, s.toCharArray.toList)
  }
}
