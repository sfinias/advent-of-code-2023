package io.sfinias.advent

import scala.collection.immutable.NumericRange
import scala.io.Source

object Fertilizer {

  def findLocation(input: Seq[String]): Long = {

    val (seeds, conversions) = input.foldLeft(Set[Long](), Map[NumericRange[Long], Long => Long]()) {
      case ((seeds, conversions), line) =>
        val matches = "(\\d+)".r.findAllMatchIn(line).map(_.group(1).toLong).toSeq
        if (matches.nonEmpty) {
          if (seeds.isEmpty) (matches.toSet, conversions)
          else {
            val target = matches.head
            val source = matches(1)
            val step = matches(2)
            val range = source until source + step
            val function: Long => Long = (number: Long) => number - (source - target)
            (seeds, conversions + (range -> function))
          }
        } else if (conversions.nonEmpty) {
          val newSeeds = seeds.foldLeft(Set[Long]()) { case (newSet, seed) =>
            val newSeed = conversions.find(_._1.contains(seed)).map(_._2(seed)).getOrElse(seed)
            newSet + newSeed
          }
          (newSeeds, Map())

        } else (seeds, conversions)
    }

    val finalSeeds = seeds.foldLeft(Set[Long]()) { case (newSet, seed) =>
      val newSeed = conversions.find(_._1.contains(seed)).map(_._2(seed)).getOrElse(seed)
      newSet + newSeed
    }
    finalSeeds.min
  }

  def findLocationWithRanges(input: Seq[String]): Long = {
    val (seeds, conversions) = input.foldLeft(Set[(Long, Long)](), Map[(Long, Long), Long]()) {
      case ((seeds, conversions), line) =>
        val matches = "(\\d+)".r.findAllMatchIn(line).map(_.group(1).toLong).toSeq
        if (matches.nonEmpty) {
          if (seeds.isEmpty) {
            val firstSeeds = matches.sliding(2, 2).map(seq => seq.head -> (seq.head + seq.last - 1)).toSet
            (firstSeeds, conversions)
          } else {
            val target = matches.head
            val source = matches(1)
            val step = matches(2)
            val sourceRange = source -> (source + step - 1)
            val stepFunction = source - target
            (seeds, conversions + (sourceRange -> stepFunction))
          }
        } else if (conversions.nonEmpty) {
          val (untouchedRanges, convertedRanges) = conversions.foldLeft(seeds, Set[(Long, Long)]()) { case ((notConverted, converted), conversion) =>
            val (conversionRange, step) = conversion

            val (unconverted, toBeAdded) = notConverted.foldLeft(notConverted, Set[(Long, Long)]()) {
              case ((stillSeeds, evolvedSeeds), originalRange) =>
                // conversion intersects with the original range
                if (conversionRange._2 < originalRange._1 || conversionRange._1 > originalRange._2) {
                  // conversion does not intersect with the original range
                  (stillSeeds, evolvedSeeds)
                } else if (conversionRange._1 <= originalRange._1 && conversionRange._2 >= originalRange._2) {
                  //full replace the original
                  val newRange = (originalRange._1 - step, originalRange._2 - step)
                  (stillSeeds - originalRange, evolvedSeeds + newRange)
                } else if (conversionRange._1 <= originalRange._1 && conversionRange._2 < originalRange._2) {
                  //keep the original end
                  val newRange = (originalRange._1 - step, conversionRange._2 - step)
                  val remainingRange = (conversionRange._2 + 1, originalRange._2)
                  (stillSeeds - originalRange + remainingRange, evolvedSeeds + newRange)
                } else if (conversionRange._1 > originalRange._1 && conversionRange._2 >= originalRange._2) {
                  //keep the original start
                  val remainingRange = (originalRange._1, conversionRange._1 - 1)
                  val newRange = (conversionRange._1 - step, originalRange._2 - step)
                  (stillSeeds - originalRange + remainingRange, evolvedSeeds + newRange)
                } else if (conversionRange._1 > originalRange._1 && conversionRange._2 < originalRange._2) {
                  val remainingRange1 = (originalRange._1, conversionRange._1 - 1)
                  val newRange = (conversionRange._1 - step, conversionRange._2 - step)
                  val remainingRange2 = (conversionRange._2 + 1, originalRange._2)
                  (((stillSeeds - originalRange) + remainingRange1) + remainingRange2, evolvedSeeds + newRange)
                } else {
                  (stillSeeds, evolvedSeeds)
                }
            }
            (unconverted, converted ++ toBeAdded)
          }
          (untouchedRanges ++ convertedRanges, Map())
        }
        else (seeds, conversions)
    }
    seeds.map(_._1).min
  }


  def main(args: Array[String]): Unit =
    println(findLocationWithRanges(Source.fromResource("fertilizer.txt").getLines().toSeq))

}