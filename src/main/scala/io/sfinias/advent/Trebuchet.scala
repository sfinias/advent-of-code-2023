package io.sfinias.advent

import scala.io.Source

object Trebuchet {

  def getCalibration(lines: Seq[String]): Int = {
    val calibrationValues = for (
      line <- lines
    ) yield line.foldLeft(0)((sum, char) =>
      if (char.isDigit) {
        sum match {
          case 0 => char.asDigit * 11
          case afterFirst => (afterFirst / 10 * 10) + char.asDigit
        }
      } else sum
    )
    calibrationValues.sum
  }


  def main(args: Array[String]): Unit = {
    val result = getCalibration(Source.fromResource("input.txt").getLines().toSeq)
    println(result)
  }
}