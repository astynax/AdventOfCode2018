package me.astynax.aoc2018

object Day02 {
  private type Input = List[String]

  def step1(items: Input): Int =
    items.foldLeft((0, 0)) { case ((n2, n3), word) =>
      val fs = frequencies(word).values.toSet
      ( if (fs contains 2) n2 + 1 else n2,
        if (fs contains 3) n3 + 1 else n3
      )
    } match {
      case (n2, n3) => n2 * n3
    }

  def step2(items: Input): String = {
    items.foreach { l1 =>
      items.foreach { l2 =>
        val pairs = l1 zip l2
        if (pairs.count({ case (c1, c2) => c1 != c2}) == 1) {
          return pairs.filter { case (c1, c2) => c1 == c2 }.map(_._1).mkString
        }
      }
    }
    ""
  }

  private def frequencies(word: String): Map[Char, Int] =
    word.toSeq.foldLeft(Map.empty[Char, Int]) { (m, v) =>
      m.updatedWith(v) { _.map(_ + 1).orElse(Some(1)) }
    }

  lazy val input: Input = Inputs.readLinesFrom("inputs/Day02.input")
}
