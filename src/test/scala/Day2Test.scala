package nl.kransen.aoc2025

import nl.kransen.aoc2025.Day2.isPattern
import org.scalatest.funsuite.AnyFunSuiteLike

class Day2Test extends AnyFunSuiteLike {

  test("testIsPattern") {
    assert(!isPattern(101))

    assert(!isPattern(422442240))

    assert(isPattern(2121212121))
  }


}
