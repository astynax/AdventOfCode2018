package me.astynax.aoc2018

import scala.annotation.tailrec

object Day01 {

  def step1(changes: List[Int]): Int = changes.sum

  def step2(changes: List[Int]): Int = {
    @tailrec
    def go(seen: Set[Int], state: Int, input: List[Int]): Int =
      if (seen contains state) state
      else input match {
        case x :: xs => go(seen + state, state + x, xs)
        case _ => go(seen, state, changes)
      }
    go(Set.empty, 0, changes)
  }

  lazy val input: List[Int] =
    Inputs.readLinesFrom("inputs/Day01.txt")
      .map(Integer.parseInt)
}
