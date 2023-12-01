package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

class InputsTest extends AnyFunSuiteLike {
  test("Basic functionality") {
    assert(Inputs.readLinesFrom("Day01.input").nonEmpty)
  }
}
