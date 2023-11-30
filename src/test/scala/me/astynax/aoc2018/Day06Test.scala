package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

import Day06._


class Day06Test extends AnyFunSuiteLike {

  test("single line decoding") {
    assert(decode("123, 456") contains Pos(123, 456))
  }

  test("input parsing") {
    assert(input.nonEmpty)
  }

  test("solution for example") {
    assert(step1(List(
      Pos(1, 1),
      Pos(1, 6),
      Pos(8, 3),
      Pos(3, 4),
      Pos(5, 5),
      Pos(8, 9),
    ), threshold=32) == (17, 16))
  }

  test("solution for input") {
    assert(step1(input) == (3909, 36238))
  }
}
