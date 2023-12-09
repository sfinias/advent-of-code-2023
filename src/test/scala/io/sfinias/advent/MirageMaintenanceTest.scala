package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MirageMaintenanceTest extends AnyWordSpec with Matchers {

  "MirageMaintenance" should {

    "calculate the example right" in {

      val example = Seq(
          "0 3 6 9 12 15",
          "1 3 6 10 15 21",
          "10 13 16 21 30 45",
      )

      MirageMaintenance.extrapolateValues(example) shouldBe 114
    }

    "calculate with negative values" in {
      val example = "-6 -9 -12 -15 -18 -21 -24 -27 -30 -33 -36 -39 -42 -45 -48 -51 -54 -57 -60 -63 -66"
      MirageMaintenance.extrapolateValues(Seq(example)) shouldBe -69
    }


    "calculate the example for prepended values right" in {

      val example = Seq(
        "0 3 6 9 12 15",
        "1 3 6 10 15 21",
        "10 13 16 21 30 45",
      )

      MirageMaintenance.extrapolatePrependedValues(example) shouldBe 2
    }
  }
}
