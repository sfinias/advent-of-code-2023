package io.sfinias.advent

import scala.io.Source

object FixedTrebuchet {

  private val wordDict: Map[String, Int] = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  private val dictionary: Seq[String] = wordDict.keys.flatMap(word =>
    (0 until word.length).map(index => word.substring(0, index + 1))
  ).toSeq


  def getCalibration(lines: Seq[String]): Int = {


    def calculateNewSum(char: Int, sum: Int) = {
      sum match {
        case 0 => char * 11
        case afterFirst => (afterFirst / 10 * 10) + char
      }
    }

    val values = for (
      line <- lines
    ) yield {
      var sum = 0
      var start = 0
      var end = 0
      while (end < line.length) {
        if (line.charAt(end).isDigit) {
          sum = calculateNewSum(line.charAt(end).asDigit, sum)
          end += 1
          start = end
        } else if (end - start + 1 < 3)
          end += 1
        else {
          val sub = line.substring(start, end + 1)
          if (dictionary.contains(sub))
            if (wordDict.contains(sub)) {
              sum = calculateNewSum(wordDict(sub), sum)
              start += 1
            } else
              end += 1
          else start += 1
        }
      }
      sum
    }
    values.sum
  }

  def main(args: Array[String]): Unit =
    println(getCalibration(Source.fromResource("input.txt").getLines().toSeq))

}
