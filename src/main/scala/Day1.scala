package nl.kransen.aoc2025

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  case class DialState(position: Int = 50, zeroStops: Int = 0, zeroPasses: Int = 0) {

    val SIZE = 100

    enum Direction:
      case LEFT, RIGHT

    def rotate(rotation: String): DialState = {
      val direction = rotation.head
      val distance = rotation.tail.toInt

      direction match {
        case 'L' => rotate(this, distance, Direction.LEFT)
        case 'R' => rotate(this, distance, Direction.RIGHT)
      }
    }

    @tailrec
    private def rotate(state: DialState, distance: Int, direction: Direction): DialState = {
      if (distance == 0) {
        if (state.position == 0) {
          state.copy(zeroStops = state.zeroStops + 1)
        } else {
          state
        }
      } else {
        val newPosition = direction match {
          case Direction.LEFT =>
            if (state.position == 0) {
              SIZE - 1
            } else {
              state.position - 1
            }
          case Direction.RIGHT =>
            if (state.position == SIZE - 1) {
              0
            } else {
              state.position + 1
            }
        }
        val newZeroPasses = if (newPosition == 0) state.zeroPasses + 1 else state.zeroPasses
        rotate(state.copy(position = newPosition, zeroPasses = newZeroPasses), distance = distance - 1, direction)
      }
    }
  }

  val testInput: String =
    """
      |L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin

//  val lines = Source.fromString(testInput).getLines().filter(_.nonEmpty)

  val lines = Source.fromResource("day1_1.txt").getLines().toList

  val testOutput = lines.foldLeft(DialState())((dialState, rotation) =>
    dialState.rotate(rotation)
  )

  println(s"Zero stops: ${testOutput.zeroStops}")

  println(s"Zero passes: ${testOutput.zeroPasses}")

  println(s"Sum: ${testOutput.zeroStops + testOutput.zeroPasses}")

}
