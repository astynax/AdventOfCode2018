package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

import Day05._

class Day05Test extends AnyFunSuiteLike {

  private val example = "dabAcCaCBAcCcaDA".toList

  test("char reactivity") {
    assert(areReacting('a', 'A'))
    assert(areReacting('Z', 'z'))
    assert(!areReacting('y', 'y'))
    assert(!areReacting('Y', 'Y'))
    assert(!areReacting('a', 'B'))
    assert(!areReacting('1', 'b'))
  }

  test("single reaction") {
    def step(s: String) = oneReaction(s.toList).map(_.mkString)
    assert(step("aBbAcC") contains "aAcC")
    assert(step("a").isEmpty)
    assert(step("aB").isEmpty)
    assert(step("aBAb").isEmpty)
  }

  test("example reactions") {
    assert(react(example).mkString == "dabCBAcaDA")
  }

  test("step1 on examples") {
    assert(step1(example) == 10)
  }

  test("step1 on input") {
    assert(step1(input) == 9386)
  }

  test("step2 on examples") {
    assert(step2(example) == 4)
  }

  test("step2 on input") {
    assert(step2(input) == 4876)
  }
}
