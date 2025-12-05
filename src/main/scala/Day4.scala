package nl.kransen.aoc2025

import scala.io.Source

object Day4 extends App {

  val testInput: String = """
                   |..@@.@@@@.
                   |@@@.@.@.@@
                   |@@@@@.@.@@
                   |@.@@@@..@.
                   |@@.@@@@.@@
                   |.@@@@@@@.@
                   |.@.@.@.@@@
                   |@.@@@.@@@@
                   |.@@@@@@@@.
                   |@.@.@@@.@.""".stripMargin

  val lines: List[String] = testInput.linesIterator.filterNot(_.isBlank).toList

//  val lines: List[String] = Source.fromResource("day4.txt").getLines().toList

  val grid: Array[Array[Boolean]] = lines.map(_.map(_ == '@').toArray).toArray
  val height: Int = grid.length
  val width: Int = grid.head.length
  println(s"height: $height, width: $width")

//  roles.foreach(println)

  val accessible = (0 until height).flatMap { row => 
    (0 until width).map(column => {
      grid(row)(column) && countNeighbours(row, column) < 4
    })
  }.count(identity)

  def countNeighbours(row: Int, col: Int): Int = {
    val count = (Math.max(0, row - 1) to Math.min(height - 1, row + 1)).flatMap { y => 
      (Math.max(0, col - 1) to Math.min(width - 1, col + 1))
        .filterNot(x => x == col && y == row)
        .map(x => grid(y)(x))
    }.count(identity)
    println(s"count: $count")
    count
  }
  
  println(s"accessible: $accessible")
}
