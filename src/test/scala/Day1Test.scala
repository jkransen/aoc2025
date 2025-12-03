package nl.kransen.aoc2025

import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  test("countZeroPasses van positie 50, 85 naar links is 1") {
    assert(Day1.countZeroPasses(50, 'L', 85) == 1)
  }

  test("countZeroPasses van positie 82, 30 naar links is 1") {
    assert(Day1.countZeroPasses(82, 'L', 30) == 0)
  }

  test("countZeroPasses van positie 150, 250 naar links is 2") {
    assert(Day1.countZeroPasses(150, 'L', 250) == 2)
  }

  test("countZeroPasses van positie 20, 220 naar rechts is 2") {
    assert(Day1.countZeroPasses(20, 'R', 220) == 2)
  }

  test("countZeroPasses van positie 50, 1000 naar rechts is 10") {
    assert(Day1.countZeroPasses(50, 'R', 1000) == 10)
  }

}
