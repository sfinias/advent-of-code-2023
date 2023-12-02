package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FixedTrebuchetTest extends AnyWordSpec with Matchers {

  "FixedTrebuchet" should {

    "calculate the example correctly" in {
      val example = Seq(
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen",
      )

      FixedTrebuchet.getCalibration(example) shouldBe 281
     }
  }
}
