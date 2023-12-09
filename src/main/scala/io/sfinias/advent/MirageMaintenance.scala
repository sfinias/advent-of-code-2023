package io.sfinias.advent

import scala.io.Source

object MirageMaintenance {


  def extrapolateValues(input: Seq[String]): Int =
    (for {
      line <- input
      history = "(-*\\d+)".r.findAllMatchIn(line).map(_.group(1).toInt).toSeq
    } yield {
      var notInTheEnd = true
      var stack = Seq(history)
      do {
        val differences = stack.last.sliding(2).map(list => list.last - list.head).toSeq
        notInTheEnd = !differences.forall(_ == 0)
        if (notInTheEnd) stack :+= differences
      } while (notInTheEnd)
      val predictedValue = stack.foldRight(0)((currentList, difference) => {
        val dif = currentList.last + difference
        dif
      })
      predictedValue
    }).sum


  def extrapolatePrependedValues(input: Seq[String]): Int =
    (for {
      line <- input
      history = "(-*\\d+)".r.findAllMatchIn(line).map(_.group(1).toInt).toSeq
    } yield {
      var notInTheEnd = true
      var stack = Seq(history)
      do {
        val differences = stack.last.sliding(2).map(list => list.last - list.head).toSeq
        notInTheEnd = !differences.forall(_ == 0)
        if (notInTheEnd) stack :+= differences
      } while (notInTheEnd)
      val predictedValue = stack.foldRight(0)((currentList, difference) => {
        val dif = currentList.head - difference
        dif
      })
      predictedValue
    }).sum

  def main(args: Array[String]): Unit =
    println(extrapolatePrependedValues(Source.fromResource("mirage-maintenance.txt").getLines().toSeq))


}
