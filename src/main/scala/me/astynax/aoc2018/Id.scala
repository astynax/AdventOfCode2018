package me.astynax.aoc2018

case class Id(value: Int)

object Id {
  def zipWithIds[A](source: Iterable[A]): Iterable[(A, Id)] =
    source.zipWithIndex.map { case (x, i) => x -> Id(i) }
}
