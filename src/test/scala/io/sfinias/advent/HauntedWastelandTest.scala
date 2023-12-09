package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HauntedWastelandTest extends AnyWordSpec with Matchers {

  "HauntedWasteland" should {

    "calculate the examples right" in {

      val example1 = Seq(
        "RL",
        "",
        "AAA = (BBB, CCC)",
        "BBB = (DDD, EEE)",
        "CCC = (ZZZ, GGG)",
        "DDD = (DDD, DDD)",
        "EEE = (EEE, EEE)",
        "GGG = (GGG, GGG)",
        "ZZZ = (ZZZ, ZZZ)"
      )

      HauntedWasteland.calculateSteps(example1) shouldBe 2

      val example2 = Seq(
        "LLR",
        "",
        "AAA = (BBB, BBB)",
        "BBB = (AAA, ZZZ)",
        "ZZZ = (ZZZ, ZZZ)",
      )

      HauntedWasteland.calculateSteps(example2) shouldBe 6
    }

    "calculate the examples for all paths right" in {

      val example = Seq(
        "LR",
        "",
        "11A = (11B, XXX)",
        "11B = (XXX, 11Z)",
        "11Z = (11B, XXX)",
        "22A = (22B, XXX)",
        "22B = (22C, 22C)",
        "22C = (22Z, 22Z)",
        "22Z = (22B, 22B)",
        "XXX = (XXX, XXX)"
      )

      HauntedWasteland.calculateStepsForAllPaths(example) shouldBe 6
    }
  }


}
