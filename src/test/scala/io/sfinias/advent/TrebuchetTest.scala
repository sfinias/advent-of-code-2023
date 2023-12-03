package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TrebuchetTest extends AnyWordSpec with Matchers {


  "A Trebuchet" should {

    "calculate the example value correctly" in {

      val list = Seq(
        "1abc2",
        "pqr3stu8vwx",
        "a1b2c3d4e5f",
        "treb7uchet"
      )

      Trebuchet.getCalibration(list) shouldBe 142
    }

    "calculate the second part of the example correctly" in {
      val example = Seq(
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen",
      )

      Trebuchet.getNewCalibration(example) shouldBe 281
    }
  }
}
