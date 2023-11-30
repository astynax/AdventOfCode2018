package me.astynax.aoc2018

import org.scalatest.funsuite.AnyFunSuiteLike

class PosTest extends AnyFunSuiteLike {

  test("distanceTo") {
    assert((Pos(0, 0) distanceTo Pos(10, 20)) == 30)
    assert((Pos(15, 30) distanceTo Pos(10, 20)) == 15)
    assert((Pos(10, 10) distanceTo Pos(10, 10)) == 0)
  }

}
