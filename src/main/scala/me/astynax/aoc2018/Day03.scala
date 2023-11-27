package me.astynax.aoc2018

object Day03 {

  private type Input = List[Claim]

  case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int) {
    val xx: Int = x + w - 1
    val yy: Int = y + h - 1

    def contains(point: (Int, Int)): Boolean = point match {
      case (px, py) => px >= x && px <= xx && py >= y && py <= yy
    }

    def intersects(c2: Claim): Boolean =
      Claim.collide(x, c2.x, w, c2.w) &&
        Claim.collide(y, c2.y, h, c2.h)
  }

  object Claim {
    def collide(c1: Int, c2: Int, s1: Int, s2: Int): Boolean =
      c1 == c2 ||
        c1 < c2 && c2 < (c1 + s1) ||
        c1 > c2 && c1 < (c2 + s2)
  }

  def step1(items: Input): Int = (for {
    y <- items.map(_.y).min to items.map(_.yy).max
    x <- items.map(_.x).min to items.map(_.xx).max
    pos = (x, y)
    if items.filter(_ contains pos).drop(1).nonEmpty
  } yield pos).size

  def step2(items: Input): Int = {
    val colliding = items.combinations(2).filter {
      case List(a, b) => a intersects b
    }.flatten.map(_.id).toSet
    items.find { c => !colliding.contains(c.id) }.get.id
  }

  private val rowPattern =
    raw"^#(?<id>\d+) @ (?<x>\d+),(?<y>\d+): (?<w>\d+)x(?<h>\d+)$$".r

  def decode(row: String): Option[Claim] =
    rowPattern.findFirstMatchIn(row)
      .flatMap { m => for {
        id <- Parsing.parseInt(m.group("id"))
        x <- Parsing.parseInt(m.group("x"))
        y <- Parsing.parseInt(m.group("y"))
        w <- Parsing.parseInt(m.group("w"))
        h <- Parsing.parseInt(m.group("h"))
      } yield Claim(id, x, y, w, h) }

  lazy val input: Input =
    Inputs.readLinesFrom("inputs/Day03.input").map(decode(_).get)
}
