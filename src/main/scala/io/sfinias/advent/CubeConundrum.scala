package io.sfinias.advent

import scala.io.Source
import scala.util.matching.Regex

object CubeConundrum {

  private val gamePattern  = """Game (\d+):""".r
  private val bluePattern = """(\d+) blue""".r
  private val greenPattern = """(\d+) green""".r
  private val redPattern = """(\d+) red""".r

  private val totalReds = 12
  private val totalGreens = 13
  private val totalBlues = 14

  def validateCubes(games: Seq[String]): Int = {
    val validGameIds = for (
      game <- games
    ) yield {
      val gameId = gamePattern.findFirstMatchIn(game).map(_.group(1)).get.toInt
      val blues = bluePattern.findAllMatchIn(game).map(_.group(1).toInt)
      val reds = redPattern.findAllMatchIn(game).map(_.group(1).toInt)
      val greens = greenPattern.findAllMatchIn(game).map(_.group(1).toInt)
      if (!blues.exists(_ > totalBlues) && !reds.exists(_ > totalReds) && !greens.exists(_ > totalGreens))
        gameId
      else 0

    }
    validGameIds.sum
  }

  def calculatePower(games: Seq[String]): Long = {
    val powers = for (
      game <- games
    ) yield {
      val gameId = gamePattern.findFirstMatchIn(game).map(_.group(1)).get.toInt
      val maxBlues = bluePattern.findAllMatchIn(game).map(_.group(1).toInt).max
      val maxReds = redPattern.findAllMatchIn(game).map(_.group(1).toInt).max
      val maxGreens = greenPattern.findAllMatchIn(game).map(_.group(1).toInt).max
      maxReds * maxGreens * maxBlues
    }
    powers.sum
  }

  def main(args: Array[String]): Unit =
    println(calculatePower(Source.fromResource("cube-conundrum.txt").getLines().toSeq))


}
