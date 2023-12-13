package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PipeMazeTest extends AnyWordSpec with Matchers {

  "PipeMaze" should {

    "calculate the example right" in {

      val input1 = Seq(
        "-L|F7",
        "7S-7|",
        "L|7||",
        "-L-J|",
        "L|-JF")

      PipeMaze.findFurthestPoint(input1) shouldBe 4

      val input2 = Seq(
        "..F7.",
        ".FJ|.",
        "SJ.L7",
        "|F--J",
        "LJ...")

      PipeMaze.findFurthestPoint(input2) shouldBe 8
    }


    "calculate the enclosed nodes right" in {

      val input1 = Seq(
        "...........",
        ".S-------7.",
        ".|F-----7|.",
        ".||.....||.",
        ".||.....||.",
        ".|L-7.F-J|.",
        ".|..|.|..|.",
        ".L--J.L--J.",
        "..........."
      )

      PipeMaze.findEnclosedTiles(input1) shouldBe 4
//
      val input2 = Seq(
        "..........",
        ".S------7.",
        ".|F----7|.",
        ".||OOOO||.",
        ".||OOOO||.",
        ".|L-7F-J|.",
        ".|II||II|.",
        ".L--JL--J.",
        ".........."
      )

      PipeMaze.findEnclosedTiles(input2) shouldBe 4


      val input3 = Seq(

          ".F----7F7F7F7F-7....",
          ".|F--7||||||||FJ....",
          ".||.FJ||||||||L7....",
          "FJL7L7LJLJ||LJ.L-7..",
          "L--J.L7...LJS7F-7L7.",
          "....F-J..F7FJ|L7L7L7",
          "....L7.F7||L7|.L7L7|",
          ".....|FJLJ|FJ|F7|.LJ",
          "....FJL-7.||.||||...",
          "....L---J.LJ.LJLJ...",
      )

      PipeMaze.findEnclosedTiles(input3) shouldBe 8

      val input4 = Seq(

        "FF7FSF7F7F7F7F7F---7",
        "L|LJ||||||||||||F--J",
        "FL-7LJLJ||||||LJL-77",
        "F--JF--7||LJLJ7F7FJ-",
        "L---JF-JLJ.||-FJLJJ7",
        "|F|F-JF---7F7-L7L|7|",
        "|FFJF7L7F-JF7|JL---7",
        "7-L-JL7||F7|L7F-7F7|",
        "L.L7LFJ|||||FJL7||LJ",
        "L7JLJL-JLJLJL--JLJ.L",
      )

      PipeMaze.findEnclosedTiles(input4) shouldBe 10
    }
  }

}
