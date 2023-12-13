package io.sfinias.advent

import scala.io.Source

object PipeMaze {

  private sealed trait Direction

  private object West extends Direction

  private object East extends Direction

  private object South extends Direction

  private object North extends Direction

  //  private case class Point(x: Int, y: Int, entry: String, step: Int)
  private case class Point(x: Int, y: Int)

  private case class Traversal(point: Point, direction: Direction, steps: Int, hasLooped: Boolean, isDeadEnd: Boolean)
  private case class Traversal2(point: Point, direction: Direction, hasLooped: Boolean, isDeadEnd: Boolean, pipesMap: Map[Int, Seq[Int]])
  private case class PipeSection(start: Int, end: Int)

  def findFurthestPoint(input: Seq[String]): Int = {
    val xMax = input.head.length
    val yMax = input.size
    def traverseMaze(traversal: Traversal) = {
      var currentTraversal = traversal
      while (!currentTraversal.hasLooped && !currentTraversal.isDeadEnd)
        currentTraversal = move(currentTraversal)
      currentTraversal
    }

    def move(traversal: Traversal): Traversal = {
      traversal.direction match {
        case East if traversal.point.x + 1 < xMax =>
          val newPoint = Point(traversal.point.x + 1, traversal.point.y)
          input(newPoint.y)(newPoint.x) match {
            case '-' => traversal.copy(point = newPoint,direction = East,steps =  traversal.steps + 1)
            case 'J' => traversal.copy(point = newPoint,direction = North,steps =  traversal.steps + 1)
            case '7' => traversal.copy(point = newPoint,direction = South,steps =  traversal.steps + 1)
            case 'S' => traversal.copy(steps = traversal.steps + 1, hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case West if traversal.point.x - 1 >= 0 =>
          val newPoint = Point(traversal.point.x - 1, traversal.point.y)
          input(newPoint.y)(newPoint.x) match {
            case '-' => traversal.copy(point = newPoint,direction = West,steps =  traversal.steps + 1)
            case 'F' => traversal.copy(point = newPoint,direction = South,steps =  traversal.steps + 1)
            case 'L' => traversal.copy(point = newPoint,direction = North,steps =  traversal.steps + 1)
            case 'S' => traversal.copy(steps = traversal.steps + 1, hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case South if traversal.point.y + 1 < yMax =>
          val newPoint = Point(traversal.point.x, traversal.point.y + 1)
          input(newPoint.y)(newPoint.x) match {
            case '|' => traversal.copy(point = newPoint,direction = South,steps =  traversal.steps + 1)
            case 'J' => traversal.copy(point = newPoint,direction = West,steps =  traversal.steps + 1)
            case 'L' => traversal.copy(point = newPoint,direction = East,steps =  traversal.steps + 1)
            case 'S' => traversal.copy(steps = traversal.steps + 1, hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case North if traversal.point.y - 1 >= 0 =>
          val newPoint = Point(traversal.point.x, traversal.point.y - 1)
          input(newPoint.y)(newPoint.x) match {
            case '|' => traversal.copy(point = newPoint,direction = North,steps =  traversal.steps + 1)
            case '7' => traversal.copy(point = newPoint,direction = West,steps =  traversal.steps + 1)
            case 'F' => traversal.copy(point = newPoint,direction = East,steps =  traversal.steps + 1)
            case 'S' => traversal.copy(steps = traversal.steps + 1, hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case _ => traversal.copy(isDeadEnd = true)
      }
    }

    val (x,y) = input.zipWithIndex.map(line => line._1.indexOf('S') -> line._2).find(_._1 >= 0).get
    val start = Point(x,y)
    val possibleTraversals = Seq(West, East, North, West).map(Traversal(start, _, 0, hasLooped = false, isDeadEnd = false))

    val loop = possibleTraversals.find(traverseMaze(_).hasLooped).get
    traverseMaze(loop).steps / 2
  }

  def findEnclosedTiles(input: Seq[String]): Int = {
    val xMax = input.head.length
    val yMax = input.size
    def traverseMaze(traversal: Traversal2) = {
      var currentTraversal = traversal
      while (!currentTraversal.hasLooped && !currentTraversal.isDeadEnd)
        currentTraversal = move(currentTraversal)
      currentTraversal
    }

    def updateMap(map: Map[Int, Seq[Int]], y: Int, x: Int) =
      map + (y -> map.get(y).map(_ :+ x).getOrElse(Seq(x)))

    def move(traversal: Traversal2) = {
      traversal.direction match {
        case East if traversal.point.x + 1 < xMax =>
          val newPoint = Point(traversal.point.x + 1, traversal.point.y)
          input(newPoint.y)(newPoint.x) match {
            case '-' => traversal.copy(point = newPoint,direction = East,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'J' => traversal.copy(point = newPoint,direction = North,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case '7' => traversal.copy(point = newPoint,direction = South,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'S' => traversal.copy(
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
              hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case West if traversal.point.x - 1 >= 0 =>
          val newPoint = Point(traversal.point.x - 1, traversal.point.y)
          input(newPoint.y)(newPoint.x) match {
            case '-' => traversal.copy(point = newPoint,direction = West,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'F' => traversal.copy(point = newPoint,direction = South,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'L' => traversal.copy(point = newPoint,direction = North,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'S' => traversal.copy(
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
              hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case South if traversal.point.y + 1 < yMax =>
          val newPoint = Point(traversal.point.x, traversal.point.y + 1)
          input(newPoint.y)(newPoint.x) match {
            case '|' => traversal.copy(point = newPoint,direction = South,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'J' => traversal.copy(point = newPoint,direction = West,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'L' => traversal.copy(point = newPoint,direction = East,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'S' => traversal.copy(
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
              hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case North if traversal.point.y - 1 >= 0 =>
          val newPoint = Point(traversal.point.x, traversal.point.y - 1)
          input(newPoint.y)(newPoint.x) match {
            case '|' => traversal.copy(point = newPoint,direction = North,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case '7' => traversal.copy(point = newPoint,direction = West,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'F' => traversal.copy(point = newPoint,direction = East,
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
            )
            case 'S' => traversal.copy(
              pipesMap = updateMap(traversal.pipesMap, newPoint.y, newPoint.x),
              hasLooped = true)
            case _ => traversal.copy(isDeadEnd = true)
          }
        case _ => traversal.copy(isDeadEnd = true)
      }
    }

    def updateMatrix(lastPoint: Traversal2, startPoint: Point): Seq[String] = {
      val y = startPoint.y
      val x = startPoint.x
      val startCharacter = lastPoint.direction match {
        case North =>
          if (lastPoint.pipesMap(y).contains(x - 1) && Seq('L', 'F', '-').contains(input(y)(x - 1))) '7'
          else if (lastPoint.pipesMap(y).contains(x + 1) && Seq('J', '7', '-').contains(input(y)(x + 1))) 'F'
          else '|'
        case South =>
          if (lastPoint.pipesMap(y).contains(x - 1) && Seq('L', 'F', '-').contains(input(y)(x - 1))) 'J'
          else if (lastPoint.pipesMap(y).contains(x + 1) && Seq('J', '7', '-').contains(input(y)(x + 1))) 'L'
          else '|'
        case East =>
          if (lastPoint.pipesMap.get(y - 1).exists(_.contains(x)) && Seq('7', 'F', '|').contains(input(y)(x - 1))) 'J'
          else if (lastPoint.pipesMap.get(y + 1).exists(_.contains(x)) && Seq('J', 'L', '|').contains(input(y)(x - 1))) '7'
          else '-'
        case _ =>
          if (lastPoint.pipesMap.get(y - 1).exists(_.contains(x)) && Seq('7', 'F', '|').contains(input(y)(x - 1))) 'L'
          else if (lastPoint.pipesMap.get(y + 1).exists(_.contains(x)) && Seq('J', 'L', '|').contains(input(y)(x - 1))) 'F'
          else '-'
      }
      val newStartLine = input(y).replace('S', startCharacter)
      input.updated(y, newStartLine)
    }

    val (x,y) = input.zipWithIndex.map(line => line._1.indexOf('S') -> line._2).find(_._1 >= 0).get
    val start = Point(x,y)
    val possibleTraversals = Seq(West, East, North, West).map(Traversal2(start, _, hasLooped = false, isDeadEnd = false, Map()))

    val loop = possibleTraversals.map(traverseMaze).find(_.hasLooped).get
    val updatedMatrix = updateMatrix(loop, start)
    loop.pipesMap.foldLeft(0) { case (totalTiles, (y, pipes)) =>
      val sorted = pipes.sorted
      val (pipeSections, _, _) = sorted.foldLeft(Seq[PipeSection](), 0, East: Direction){ case ((pipeSections, startOfPipe, direction), x) =>
        updatedMatrix(y)(x) match {
          case '|' => (pipeSections :+ PipeSection(x, x), startOfPipe, direction)
          case 'L' => (pipeSections, x, North)
          case 'F' => (pipeSections, x, South)
          case '7' if direction == North => (pipeSections :+ PipeSection(startOfPipe, x), startOfPipe, direction)
          case 'J' if direction == South => (pipeSections :+ PipeSection(startOfPipe, x), startOfPipe, direction)
          case _ => (pipeSections, startOfPipe, direction)
        }
      }
      val tiles = pipeSections.sliding(2, 2).map{ x =>
        (x.head.end + 1 until x.last.start).foldLeft(0)((acc, tile) =>
          if (sorted.contains(tile)) acc else acc + 1
        )
      }.sum
      totalTiles + tiles
    }
  }


  def main(args: Array[String]): Unit =
    println(findEnclosedTiles(Source.fromResource("pipe-maze.txt").getLines().toSeq))


}
