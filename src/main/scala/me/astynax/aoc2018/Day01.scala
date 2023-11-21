package me.astynax.aoc2018

object Day01 {

  def step1(changes: List[Int]): Int = changes.sum

  def step2(changes: List[Int]): Int = {
    val seen = scala.collection.mutable.Set[Int]()
    var sum = 0
    while (true) {
      changes.foreach { i =>
        sum += i
        if (seen contains sum) {
          return sum
        }
        seen.add(sum)
      }
    }
    0  // impossible
  }

  lazy val input: List[Int] =
    Inputs.readLinesFrom("inputs/Day01.txt")
      .map(Integer.parseInt)
}
