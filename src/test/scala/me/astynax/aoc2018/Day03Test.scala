package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

class Day03Test extends AnyFunSuiteLike {

  test("decoding") {
    assert(Day03.decode("#123 @ 45,12 55x33")
      contains Day03.Claim(123, 45, 12, 55, 33))
  }

  test("Claim.collide") {
    assert(Day03.Claim.collide(3, 6, 4, 4))
    assert(Day03.Claim.collide(6, 3, 4, 4))
    assert(Day03.Claim.collide(3, 3, 4, 2))
    assert(!Day03.Claim.collide(3, 6, 2, 2))
    assert(Day03.Claim.collide(3, 6, 10, 2))
    assert(Day03.Claim.collide(6, 3, 2, 10))
  }

  test("step1 on examples") {
    val example = List(
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 5,5: 2x2",
    ).map(Day03.decode(_).get)
    assert(Day03.step1(example) == 4)
    val example2 = List(
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 4,4: 3x3",
      "#3 @ 4,3: 3x2",
    ).map(Day03.decode(_).get)
    assert(Day03.step1(example2) == 10)
  }

  test("step1 on input") {
    assert(Day03.step1(Day03.input) == 109716)
  }

  test("step2 on example") {
    val example = List(
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 5,5: 2x2",
    ).map(Day03.decode(_).get)
    assert(Day03.step2(example) == 3)
  }

  test("step2 on input") {
    assert(Day03.step2(Day03.input) == 124)
  }
}
