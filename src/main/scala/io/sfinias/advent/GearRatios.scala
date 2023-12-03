package io.sfinias.advent

import scala.io.Source

object GearRatios {


  private val numberPattern = """(\d+)""".r
  private val asteriskPattern = """(\*)""".r

  def calculatePartNumbers(schematic: Seq[String]): Int = {


    def isNumberAdjacentToSymbol(xIndex: Int, yIndex: Int, end: Int, lineSize: Int) = {
      val values = for {
        y <- yIndex - 1 to yIndex + 1
        if y > 0 && y < schematic.size
        x <- xIndex - 1 to end
        if x > 0 && x < lineSize
        if (y != yIndex) || (y == yIndex && (x == xIndex - 1 || x == end))
      } yield {
        val symb = schematic(y)(x)
        !symb.isDigit && !symb.isLetter && symb != '.'
      }
      values.contains(true)
    }

    schematic.zipWithIndex.foldLeft(0) { case (acc, (line, index)) =>
      acc + numberPattern.findAllMatchIn(line)
        .filter(number => isNumberAdjacentToSymbol(number.start, index, number.end, line.length))
        .map(_.group(1).toInt).sum
    }
  }

  def calculateGears(schematic: Seq[String]): Int = {
    val (numbers, gears) = schematic.zipWithIndex.foldLeft((Map[Int, Set[Range]](), Set[(Int, Int)]())) {
      case ((numbersMap, gears), (line, index)) =>
        (numbersMap + (index -> numberPattern.findAllMatchIn(line).map(mat => Range(mat.start, mat.end)).toSet),
          gears ++ asteriskPattern.findAllMatchIn(line).map(mat => (index, mat.start)),
        )
    }
    val (sumWithGears, _) = {
      val values = for {
        gear <- gears
        y <- gear._1 - 1 to gear._1 + 1
        if y >= 0 && y < schematic.size
        x <- gear._2 - 1 to gear._2 + 1
        if x >= 0 && x < schematic.head.length
        if y != gear._1 || x == gear._2 - 1 || x == gear._2 + 1
        range <- numbers(y)
        if range.contains(x)
      } yield {
        val number = schematic(y).substring(range.start, range.end).toInt
        (gear, y, range, number)
      }
      val gearsAndValues = values.map(x => x._1 -> (x._2, x._3, x._4)).groupBy(_._1)
        .map { case (k, v) => k -> v.map(_._2) }
      gearsAndValues.foldLeft((0, numbers)) { case ((acc, remainingNumbers), (_, set)) =>
        val newSum = if (set.size == 2) acc + set.map(_._3).product else acc
        val newNumbers = set.foldLeft(remainingNumbers) { case (nums, (y, range, _)) =>
          nums + (y -> (nums(y) - range))
        }
        (newSum, newNumbers)
      }
    }
    sumWithGears
  }


  def main(args: Array[String]): Unit =
    println(calculateGears(Source.fromResource("gear-ratios.txt").getLines().toSeq))

}
