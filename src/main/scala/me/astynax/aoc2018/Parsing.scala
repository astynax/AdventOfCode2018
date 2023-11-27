package me.astynax.aoc2018


object Parsing {
  def parseInt(input: String): Option[Int] =
    try {
      Some(Integer.parseInt(input))
    } catch {
      case _: NumberFormatException => None
    }
}
