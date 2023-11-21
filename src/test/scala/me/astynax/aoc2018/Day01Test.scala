package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

class Day01Test extends AnyFunSuiteLike {
  test("step1 on examples") {
    assert(Day01.step1(List(1, -2, 3, 1)) == 3)
    assert(Day01.step1(List(1, 1, -2)) == 0)
  }

  test("step1 on input") {
    assert(Day01.step1(Day01.input) == 553)
  }

  test("step2 on examples") {
    assert(Day01.step2(List(1, -2, 3, 1)) == 2)
    assert(Day01.step2(List(3, 3, 4, -2, -4)) == 10)
  }

  test("step2 on input") {
    assert(Day01.step2(Day01.input) == 78724)
  }
}
