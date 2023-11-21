package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

class Day02Test extends AnyFunSuiteLike {

  test("step1 on examples") {
    val example = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab",
    )
    assert(Day02.step1(example) == 12)
  }

  test("step1 on input") {
    assert(Day02.step1(Day02.input) == 7192)
  }

  test("step2 on examples") {
    val example = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz",
    )
    assert(Day02.step2(example) == "fgij")
  }

  test("step2 on input") {
    assert(Day02.step2(Day02.input) == "mbruvapghxlzycbhmfqjonsie")
  }
}
