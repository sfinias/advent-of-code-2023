package io.sfinias.advent

import scala.io.Source

object Scratchcards {

  def calculatePoints(cards: Seq[String]): Int = {
    val scores = for {
      card <- cards
      winningSet = card.substring(card.indexOf(":") + 1, card.indexOf("|")).trim.split("\\s+").map(_.toInt).toSet
      playedSet = card.substring(card.indexOf("|") + 1).trim.split("\\s+").map(_.toInt).toSet
      matchedNumbers = winningSet.intersect(playedSet)

    } yield matchedNumbers.foldLeft(0) { case (acc, _) => if (acc == 0) 1 else acc * 2 }
    scores.sum
  }

  def calculateValidPoints(cards: Seq[String]): Int = {
    val (scores, _) = cards.foldLeft((0, Map[Int,Int]())) { case ((acc, copiesMap), card) =>
      val game = card.substring(4, card.indexOf(":")).trim.toInt
      val winningSet = card.substring(card.indexOf(":") + 1, card.indexOf("|")).trim.split("\\s+").map(_.toInt).toSet
      val playedSet = card.substring(card.indexOf("|") + 1).trim.split("\\s+").map(_.toInt).toSet
      val score = winningSet.intersect(playedSet).size
      val newCopiesMap = (game + 1 to Math.min(game + score, cards.size))
        .foldLeft(copiesMap) { case (map, newCopy) =>
        map + (newCopy -> (map.getOrElse(game, 1) + map.getOrElse(newCopy, 1)))
      }

      (acc + newCopiesMap.getOrElse(game, 1), newCopiesMap)
    }
    scores
  }

  def main(args: Array[String]): Unit =
    println(calculateValidPoints(Source.fromResource("scratchcard.txt").getLines().toSeq))

}
