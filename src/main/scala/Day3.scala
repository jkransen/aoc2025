package nl.kransen.aoc2025

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {

//  val input: List[String] = """
//                        |987654321111111
//                        |811111111111119
//                        |234234234234278
//                        |818181911112111""".stripMargin.linesIterator.toList

  val input: List[String] = Source.fromResource("day3.txt").getLines().toList

  def maxValue2(bankString: String): Long = {
    val bankDigits = bankString.map(_.toString).map(_.toLong)
    maxValue2(bankDigits)
  }

  @tailrec
  def maxValue2(bank: Seq[Long], previousMax: Long = 0): Long = {
    if (bank.tail.isEmpty) {
      previousMax
    } else {
      val withHead = bank.head * 10 + bank.tail.max
      maxValue2(bank.tail, math.max(withHead, previousMax))
    }
  }

  def maxValueN(selectCount: Int)(bankString: String): Long = {
    val bankDigits = bankString.map(_.toString).map(_.toLong)
    maxValueN(bankDigits, selectCount)
  }

  @tailrec
  def maxValueN(bank: Seq[Long], selectCount: Int, accumulated: Long = 0): Long = {
    if (selectCount == 0) {
      accumulated
    } else {
      val maxHead = bank.dropRight(selectCount - 1).max
      val maxHeadIndex = bank.indexWhere(_ == maxHead)
      maxValueN(bank.drop(maxHeadIndex + 1), selectCount - 1, 10 * accumulated + maxHead)
    }
  }

  val peaks2: List[Long] = input.filterNot(_.isBlank).map(maxValue2)
  peaks2.foreach(println)
  println(peaks2.sum)

  val peaks12: List[Long] = input.filterNot(_.isBlank).map(maxValueN(12))
  peaks12.foreach(println)
  println(peaks12.sum)
}
