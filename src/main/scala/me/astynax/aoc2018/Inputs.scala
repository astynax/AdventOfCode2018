package me.astynax.aoc2018

import scala.util.Using

object Inputs {
  def readLinesFrom(path: String): List[String] =
    Using(
      scala.io.Source.fromResource(path)
    ) {
      _.getLines().toList
    }.get
}
