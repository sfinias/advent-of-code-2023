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


  def calculateStepsForAllPaths(input: Seq[String]): Int = {

    val path = input.head
    val map = input.map(roadPattern.findAllMatchIn)
      .flatMap(_.map(matched => matched.group(1) -> (matched.group(2), matched.group(3))))
      .toMap

    var currentPoints = map.keys.map(startingPointPattern.findAllMatchIn).flatMap(_.map(_.group(1))).toSet
    var i = 0
    var stepsTaken = 0
    while (!currentPoints.forall(_.endsWith("Z")) || i < path.length) {
      if (i == path.length) i = 0
      stepsTaken += 1
      val turn = path(i)
      val newPoints = for {
        point <- currentPoints
        newPoint = if (turn == 'L') map(point)._1 else map(point)._2
      } yield newPoint
      currentPoints = newPoints
      i += 1
    }
    stepsTaken
  }


  def main(args: Array[String]): Unit =
    println(calculateStepsForAllPaths(Source.fromResource("haunted-wasteland.txt").getLines().toSeq))


}
