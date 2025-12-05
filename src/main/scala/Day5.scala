package nl.kransen.aoc2025

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  val testInput: String = """3-5
      |10-14
      |16-20
      |12-18
      |
      |1
      |5
      |8
      |11
      |17
      |32""".stripMargin
      
//    val lines = testInput.linesIterator.toList
  val lines: List[String] = Source.fromResource("day5.txt").getLines().toList

  val blankLineIndex = lines.indexWhere(_.isBlank)

  val (rangesStrings, ingredientsStrings) = lines.splitAt(blankLineIndex)
  
  val ingredients = ingredientsStrings.filterNot(_.isBlank).map(_.toLong)
  ingredients.foreach(println)
  
  case class Range(from: Long, to: Long) extends Ordered[Range] {
    val size: Long = to - from + 1

    def contains(value: Long): Boolean = value >= from && value <= to
    
    def overlaps(other: Range): Boolean = {
      (other.from >= this.from && other.from <= this.to) ||
        (other.to >= this.from && other.to <= this.to)
    }
    
    def merge(other: Range): Range = {
      Range(Math.min(this.from, other.from), Math.max(this.to, other.to))
    }

    override def compare(that: Range): Int = {
      val fromCmp = this.from.compare(that.from)
      if (fromCmp != 0) fromCmp else this.to.compare(that.to)
    }

    override def toString: String = s"Range $from to $to"
  }

  val ranges = rangesStrings.map(str => {
    val (from, to) = str.splitAt(str.indexWhere(_ == '-'))
    Range(from.toLong, to.tail.toLong)
  })
  
  ranges.foreach(println)
  
  val fresh = ingredients.filter(ingredient => {
    ranges.exists(range => range.contains(ingredient))
  })
  
  println(s"Fresh count: ${fresh.length}")
  
  @tailrec
  def merge(ranges: List[Range], aggregate: List[Range] = List()): List[Range] = {
    if (ranges.isEmpty) {
      aggregate.reverse
    } else {
      val head = ranges.head
      val joined = if (aggregate.nonEmpty && aggregate.head.overlaps(head)) {
        head.merge(aggregate.head) :: aggregate.tail
      } else {
        head :: aggregate
      }
      merge(ranges.tail, joined)
    }
  }

  val sorted = ranges.sorted
  println("Sorted:")
  sorted.foreach(println)
  val merged = merge(sorted)
  println("Merged:")
  merged.foreach(println)
  val rangeSizes = merged.map(_.size).sum

  println(s"Range sizes: $rangeSizes")
}
