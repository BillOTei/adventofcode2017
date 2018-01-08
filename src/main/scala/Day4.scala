object Day4 {
  def main(args: Array[String]): Unit = {
    //    println(day4Part2())
  }

  def day4Part2(s: String): Int = {
    val r = s.split("\n").map(
      _.split(" ").map(_.sorted).groupBy(identity).foldLeft(false)(
        (check, t) => if (check) check else t._2.length > 1
      )
    )

    r.count(_ == false)
  }

  def day4Part1(s: String): Int = {
    val r = s.split("\n").map(
      _.split(" ").groupBy(identity).foldLeft(false)(
        (check, t) => if (check) check else t._2.length > 1
      )
    )

    r.count(_ == false)
  }
}
