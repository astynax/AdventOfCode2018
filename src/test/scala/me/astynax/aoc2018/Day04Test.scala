package me.astynax.aoc2018

import me.astynax.aoc2018.Day04.Decoder
import org.scalatest.funsuite.AnyFunSuiteLike

class Day04Test extends AnyFunSuiteLike {

  private val example =
    """[1518-11-01 00:00] Guard #10 begins shift
      |[1518-11-01 00:05] falls asleep
      |[1518-11-01 00:25] wakes up
      |[1518-11-01 00:30] falls asleep
      |[1518-11-01 00:55] wakes up
      |[1518-11-01 23:58] Guard #99 begins shift
      |[1518-11-02 00:40] falls asleep
      |[1518-11-02 00:50] wakes up
      |[1518-11-03 00:05] Guard #10 begins shift
      |[1518-11-03 00:24] falls asleep
      |[1518-11-03 00:29] wakes up
      |[1518-11-04 00:02] Guard #99 begins shift
      |[1518-11-04 00:36] falls asleep
      |[1518-11-04 00:46] wakes up
      |[1518-11-05 00:03] Guard #99 begins shift
      |[1518-11-05 00:45] falls asleep
      |[1518-11-05 00:55] wakes up
      |""".stripMargin.split('\n').toList

  test("Decoder.parse") {
    assert(
      Decoder.parse("[1518-07-05 23:56] Guard #2593 begins shift")
        .contains((23, 56, "Guard", Some(2593)))
    )
    assert(
      Decoder.parse("[1518-03-31 00:55] falls asleep")
        .contains((0, 55, "falls", None))
    )
    assert(
      Decoder.parse("[1518-06-03 00:46] wakes up")
        .contains((0, 46, "wakes", None))
    )
  }

  test("Decoder.fold example") {
    assert(Decoder.fold(example).size == 5)
  }

  test("Decoder.fold input") {
    assert(Day04.input.nonEmpty)
  }

  test("totalAndHot") {
    assert(Day04.totalAndHot(List(
      5 -> 25, 30 -> 55, 24 -> 29
    )) == (50, 24))
  }

  test("step1 on examples") {
    assert(Day04.step1(Day04.Decoder.fold(example)) == 240)
  }

  test("step1 on input") {
    assert(Day04.step1(Day04.input) == 35623)
  }

  test("step2 on examples") {
    assert(Day04.step2(Day04.Decoder.fold(example)) == 4455)
  }

  test("step2 on input") {
    assert(Day04.step2(Day04.input) == 23037)
  }
}
