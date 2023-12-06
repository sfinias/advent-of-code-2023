package io.sfinias.advent

import scala.io.Source

object BoatRace {

  def beatRecord(input: Seq[String]): Int = {
    val times = "(\\d+)".r.findAllMatchIn(input.head).map(_.group(1).toInt).toSeq
    val distances = "(\\d+)".r.findAllMatchIn(input.last).map(_.group(1).toInt).toSeq
    times.zip(distances).foldLeft(1){ case (accumulatedScore, (time, distance)) =>
      val firstFound = (1 to time / 2).find(x => x * (time - x) > distance).getOrElse(0)
      val score = time - 2 * firstFound + 1
      accumulatedScore * score
    }
  }

  def beatRecordWithKerning(input: Seq[String]): Long = {
    val time = "(\\d+)".r.findAllMatchIn(input.head).map(_.group(1)).mkString("").toLong
    val distance = "(\\d+)".r.findAllMatchIn(input.last).map(_.group(1)).mkString("").toLong
    val firstFound = (1L to time / 2).find(x => x * (time - x) > distance).getOrElse(0L)
    time - 2L * firstFound + 1L
  }


  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("boat-race.txt").getLines().toSeq
    val startOfPart1 = System.currentTimeMillis()
    println(s"Part 1: result: ${beatRecord(input)} time: ${System.currentTimeMillis() - startOfPart1} millis")
    val startOfPart2 = System.currentTimeMillis()
    println(s"Part 2: result: ${beatRecordWithKerning(input)} time: ${System.currentTimeMillis() - startOfPart2} millis")
  }

}
