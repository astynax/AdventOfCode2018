package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

import scala.jdk.StreamConverters.StreamHasToScala

import Day07._

class Day07Test extends AnyFunSuiteLike {

  private val example =
    """|Step C must be finished before step A can begin.
       |Step C must be finished before step F can begin.
       |Step A must be finished before step B can begin.
       |Step A must be finished before step D can begin.
       |Step B must be finished before step E can begin.
       |Step D must be finished before step E can begin.
       |Step F must be finished before step E can begin.
       |""".stripMargin.lines().toScala(List).map(decode)

  test("decoding") {
    assert(example.head == ('A', 'C'))
  }

  test("step1 on examples") {
    assert(step1(example) == "CABDFE")
  }

  test("step1 on input") {
    assert(step1(input) == "EFHLMTKQBWAPGIVXSZJRDUYONC")
  }

  test("step2 on examples") {
    assert(step2(example, workers = 2, jobTime = { c => -64 + c }) == 15)
  }

  test("step2 on input") {
    assert(step2(input) == 1056)
  }
}
