package io.sfinias.advent

import scala.io.Source

object HauntedWasteland {


  private val roadPattern = "(\\w{3}) = \\((\\w{3}), (\\w{3})\\)".r
  private val startingPointPattern = "(\\w{2}A)".r

  def calculateSteps(input: Seq[String]): Int = {

    val path = input.head
    val map = input.map(roadPattern.findAllMatchIn)
      .flatMap(_.map(matched => matched.group(1) -> (matched.group(2), matched.group(3))))
      .toMap

    var i = 0
    var current = "AAA"
    var stepsTaken = 0
    while (current != "ZZZ" || i < path.length) {
      if (i == path.length) i = 0
      stepsTaken += 1
      val turn = path(i)
      current = if (turn == 'L') map(current)._1 else map(current)._2
      i += 1
    }
    stepsTaken
  }


  def calculateStepsForAllPaths(input: Seq[String]): Long = {
    val path = input.head
    val map = input.map(roadPattern.findAllMatchIn)
      .flatMap(_.map(matched => matched.group(1) -> (matched.group(2), matched.group(3))))
      .toMap

    val startPoints = map.keys.map(startingPointPattern.findAllMatchIn).flatMap(_.map(_.group(1))).toSet
    val loops = for (start <- startPoints) yield {
      var i = 0
      var current = start
      var stepsTaken = 0
      while (!(current.endsWith("Z") && i == path.length)) {
        if (i == path.length) i = 0
        stepsTaken += 1
        val turn = path(i)
        current = if (turn == 'L') map(current)._1 else map(current)._2
        i += 1
      }
      stepsTaken
    }
    loops.map(_.toLong).reduce(lcm)
  }

  private def gcd(x: Long, y: Long): Long = {
    var a = x
    var b = y
    while (b != 0) {
      val temp = b
      b = a % b
      a = temp
    }
    a
  }

  private def lcm(x: Long, y: Long): Long =
    x * y / gcd(x, y)


  def main(args: Array[String]): Unit =
    println(calculateStepsForAllPaths(Source.fromResource("haunted-wasteland.txt").getLines().toSeq))


}
