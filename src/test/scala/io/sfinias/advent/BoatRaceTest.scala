package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BoatRaceTest extends AnyWordSpec with Matchers {

  "BoatRace" should {

    "calculate the example right" in {
      val example = Seq(
        "Time:      7  15   30",
        "Distance:  9  40  200"
      )
      BoatRace.beatRecord(example) shouldBe 288
    }

    "calculate the kerning example right" in {
      val example = Seq(
        "Time:      7  15   30",
        "Distance:  9  40  200"
      )
      BoatRace.beatRecordWithKerning(example) shouldBe 71503
    }

  }
}
