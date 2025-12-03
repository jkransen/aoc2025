package nl.kransen.aoc2025

import scala.io.Source

object Day1 extends App {

  case class DialState(position: Int = 50, zeroStops: Int = 0, zeroPasses: Int = 0) {
    def rotate(rotation: String): DialState = {
      val direction = rotation.head
      val distance = Integer.parseInt(rotation.tail)

      val newPosition = direction match {
        case 'L' => ((position - distance) % 100 + 100) % 100
        case 'R' => (position + distance) % 100
      }

      val newZeroStops = if (newPosition == 0)
        zeroStops + 1
      else
        zeroStops

      val passesCount = countZeroPasses(position, direction, distance)
      val adjustedPasses = if (newPosition == 0 && passesCount > 0) passesCount - 1 else passesCount
      val newZeroPasses = adjustedPasses + zeroPasses

      println(s"Moving ${rotation} from position ${position} to ${newPosition}, zero stops: ${newZeroStops}, zero passes: ${newZeroPasses}")

      DialState(newPosition, newZeroStops, newZeroPasses)
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

  val testOutput = lines.foldLeft(DialState())((dialState, rotation) => dialState.rotate(rotation))

  println(s"Zero stops: ${testOutput.zeroStops}")

  println(s"Zero passes: ${testOutput.zeroPasses}")

  println(s"Sum: ${testOutput.zeroStops + testOutput.zeroPasses}")

  def countZeroPasses(position: Int, direction: Char, distance: Int): Int = {
    direction match {
      case 'L' =>
        if (position == 0) distance / 100
        else distance / 100 + (distance % 100 + 99 - position) / 100
      case 'R' => (position + distance) / 100 - position / 100
    }
  }


  // 6657 is too low
  // 7243 is too high
  // 7085 ook niet goed
}
