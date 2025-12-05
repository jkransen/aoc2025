package nl.kransen.aoc2025

import scala.annotation.tailrec
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

//  val lines: List[String] = testInput.linesIterator.filterNot(_.isBlank).toList

  val lines: List[String] = Source.fromResource("day4.txt").getLines().toList
  
  private type Grid = Array[Array[Boolean]]

  val grid: Grid = lines.map(_.map(_ == '@').toArray).toArray
  val height: Int = grid.length
  val width: Int = grid.head.length
  println(s"height: $height, width: $width")

  def accessibleGrid(grid: Grid): Grid = {
    (0 until height).map { row =>
      (0 until width).map(column =>
        grid(row)(column) && countNeighbours(grid, row, column) < 4
      ).toArray
    }.toArray
  }

  def countNeighbours(grid: Grid, row: Int, col: Int): Int = {
    (Math.max(0, row - 1) to Math.min(height - 1, row + 1)).flatMap { y =>
      (Math.max(0, col - 1) to Math.min(width - 1, col + 1))
        .filterNot(x => x == col && y == row)
        .map(x => grid(y)(x))
    }.count(identity)
  }

  def updateGrid(grid: Grid, accessible: Grid): Grid = {
    (0 until height).map { row =>
      (0 until width).map(column => {
        grid(row)(column) && !accessible(row)(column)
      }).toArray
    }.toArray
  }

  def printGrid(grid: Grid): Unit = {
    println
    (0 until height).foreach { row =>
      (0 until width).foreach(column => {
        print(if (grid(row)(column)) '@' else '.')
      })
      println
    }
  }
  
  @tailrec
  def removeAllAccessible(grid: Grid, removed: Int = 0): Int = {
    printGrid(grid)
    val accessible = accessibleGrid(grid)
    val accessibleCount: Int = accessible.flatten.count(identity)
    if (accessibleCount == 0) {
      removed
    } else {
      val updatedGrid = updateGrid(grid, accessible)
      removeAllAccessible(updatedGrid, removed + accessibleCount)
    }
  }
  
  val removed = removeAllAccessible(grid)
  
  println(s"Removed: $removed")
}
