package me.astynax.aoc2018

object Inputs {
  def readLinesFrom(path: String): List[String] = {
    val file = scala.io.Source.fromFile(path)
    val data = file.getLines().toList
    file.close()
    data
  }
}
