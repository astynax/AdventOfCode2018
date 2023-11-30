package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

import Day01._

class Day01Test extends AnyFunSuiteLike {
  test("step1 on examples") {
    assert(step1(List(1, -2, 3, 1)) == 3)
    assert(step1(List(1, 1, -2)) == 0)
  }

  test("step1 on input") {
    assert(step1(input) == 553)
  }

  test("step2 on examples") {
    assert(step2(List(1, -2, 3, 1)) == 2)
    assert(step2(List(3, 3, 4, -2, -4)) == 10)
  }

  test("step2 on input") {
    assert(step2(input) == 78724)
  }
}
