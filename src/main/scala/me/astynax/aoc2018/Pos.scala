package me.astynax.aoc2018

import scala.math.abs

case class Pos(x: Int, y: Int) {
  def distanceTo(other: Pos): Int =
    abs(x - other.x) + abs(y - other.y)
}
