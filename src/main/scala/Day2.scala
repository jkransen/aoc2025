package nl.kransen.aoc2025

import scala.io.Source

object Day2 extends App {

//  val input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

  val input = Source.fromResource("day2.txt").getLines().toList.head

  val rangeStrings = input.split(",")

  case class Range(start: Long, end: Long) {
    val values: Seq[Long] = start to end
  }

  def makeRange(rangeString: String): Range = {
    val parts = rangeString.split("-")
    Range(parts(0).toLong, parts(1).toLong)
  }

  def isSymmetric(value: Long): Boolean = {
    val str = value.toString
    val len = str.length
    if (len % 2 == 1) {
      false
    } else {
      val first = str.substring(0, len / 2)
      val second = str.substring(len / 2)
      first.equals(second)
    }
  }

  def isPattern(value: Long): Boolean = {
    val str = value.toString
    val len = str.length
    (1 to len / 2)
      .filter(length => len % length == 0)
      .exists(isPattern(str))
  }

  def isPattern(value: String)(length: Int): Boolean = {
    val pattern = value.substring(0, length)
    val repetitionCount = value.length / length
    val ints = (0 until repetitionCount)
      .map(_ * length)
    ints.forall(i => value.substring(i, i + length).equals(pattern))
  }

  val ranges = rangeStrings.map(makeRange)

  val sum = ranges.flatMap(r => r.values).filter(isSymmetric).sum

  println(sum)

  val sum2 = ranges.flatMap(r => r.values).filter(isPattern).sum

  println(sum2)
}
