object Day3 {
  def main(args: Array[String]): Unit = {
    //    println(day3Part2())
  }

  def day3Part1(n: Int): Int = {
    def go(i: Int, x: Int, y: Int, size: Int): Int = {
      // Spins along the limits of each square
      if (i == n) math.abs(x) + math.abs(y)
      else if (i > size * size) go(i + 1, x, y + 1, size + 2)
      else if (y == -(size - 1) / 2) go(i + 1, x + 1, y, size)
      else if (x == -(size - 1) / 2) go(i + 1, x, y - 1, size)
      else if (y == (size - 1) / 2) go(i + 1, x - 1, y, size)
      else go(i + 1, x, y + 1, size)
    }

    go(2, 1, 0, 3)
  }

  def day3Part2(n: Int): Int = {
    def go(i: Int, prevI: Int, x: Int, y: Int, size: Int): Int = {
      // Spins along the limits of each square
      if (i > n) i
      else if (i > size * size) go(i + 1, i, x, y + 1, size + 2)
      else if (y == -(size - 1) / 2) go(i + 1, i, x + 1, y, size)
      else if (x == -(size - 1) / 2) go(i + 1, i, x, y - 1, size)
      else if (y == (size - 1) / 2) go(i + 1, i, x - 1, y, size)
      else go(i + 1, i, x, y + 1, size)
    }

    go(2, 1, 1, 0, 3)
  }
}
