package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GearRatiosTest extends AnyWordSpec with Matchers {


  "GearRatio" should {

    "calculate the example right" in {

      val example = Seq(
        "467..114..",
        "...*......",
        "..35..633.",
        "......#...",
        "617*......",
        ".....+.58.",
        "..592.....",
        "......755.",
        "...$.*....",
        ".664.598..",
      )

      GearRatios.calculatePartNumbers(example) shouldBe 4361
    }


    "calculate the example with gears right" in {

      val example = Seq(
        "467..114..",
        "...*......",
        "..35..633.",
        "......#...",
        "617*......",
        ".....+.58.",
        "..592.....",
        "......755.",
        "...$.*....",
        ".664.598..",
      )

      GearRatios.calculateGears(example) shouldBe 467835
    }
  }
}
