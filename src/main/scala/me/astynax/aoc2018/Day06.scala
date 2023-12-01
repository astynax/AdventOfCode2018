package me.astynax.aoc2018

object Day06 {
  private type Input = List[(Pos, Id)]

  def step1(items: Input, threshold: Int = 10000): (Int, Int) = {
    val minX = items.map(_._1.x).min
    val minY = items.map(_._1.y).min
    val maxX = items.map(_._1.x).max
    val maxY = items.map(_._1.y).max
    val (counts, infinite, total) = (for {
      y <- minY to maxY
      x <- minX to maxX
    } yield Pos(x, y))
      .foldLeft(
        (Map.empty[Id, Int], Set.empty[Id], 0)
      ) { (state, p) =>
        val (acc, inf, total) = state
        val distances = items
          .map { case (x, i) => (x distanceTo p, i) }
          .sortBy(_._1)
        val newTotal = if (distances.map(_._1).sum < threshold) total + 1 else total
        if (distances.size == 1 | distances.head._1 == distances.drop(1).head._1)
          (acc, inf, newTotal)
        else {
          val (_, tag) :: _ = distances
          val newAcc =
            acc.updatedWith(tag) {
              _.map(_ + 1).orElse(Some(1))
            }
          val newInf =
            if (
              p.x == minX | p.x == maxX |
                p.y == minY | p.y == maxY
            ) inf + tag
            else inf
          (newAcc, newInf, newTotal)
        }
      }
    val topFinite = counts.toSeq
      .filter { case (k, _) => !infinite.contains(k) }
      .map(_._2)
      .max
    (topFinite, total)
  }

  def step2(items: Input): Unit = println("See step1 :)")

  private val pattern = raw"^(?<x>\d+),\s*(?<y>\d+)$$".r

  def decode(str: String): Option[Pos] =
    for {
      m <- pattern.findFirstMatchIn(str)
      x <- Parsing.parseInt(m.group("x"))
      y <- Parsing.parseInt(m.group("y"))
    } yield Pos(x, y)

  lazy val input: Input =
    Id.zipWithIds(
      Inputs.readLinesFrom("inputs/Day06.input")
        .map(decode(_).get)
    ).toList
}
