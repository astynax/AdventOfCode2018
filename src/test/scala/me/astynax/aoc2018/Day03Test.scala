package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

import Day03._

class Day03Test extends AnyFunSuiteLike {

  test("decoding") {
    assert(decode("#123 @ 45,12 55x33")
      contains Claim(123, 45, 12, 55, 33))
  }

  test("Claim.collide") {
    assert(Claim.collide(3, 6, 4, 4))
    assert(Claim.collide(6, 3, 4, 4))
    assert(Claim.collide(3, 3, 4, 2))
    assert(!Claim.collide(3, 6, 2, 2))
    assert(Claim.collide(3, 6, 10, 2))
    assert(Claim.collide(6, 3, 2, 10))
  }

  test("step1 on examples") {
    val example = List(
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 5,5: 2x2",
    ).map(decode(_).get)
    assert(step1(example) == 4)
    val example2 = List(
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 4,4: 3x3",
      "#3 @ 4,3: 3x2",
    ).map(decode(_).get)
    assert(step1(example2) == 10)
  }

  test("step1 on input") {
    assert(step1(input) == 109716)
  }

  test("step2 on example") {
    val example = List(
      "#1 @ 1,3: 4x4",
      "#2 @ 3,1: 4x4",
      "#3 @ 5,5: 2x2",
    ).map(decode(_).get)
    assert(step2(example) == 3)
  }

  test("step2 on input") {
    assert(step2(input) == 124)
  }
}
