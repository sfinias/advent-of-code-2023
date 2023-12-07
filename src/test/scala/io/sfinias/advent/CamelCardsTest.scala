package io.sfinias.advent

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CamelCardsTest extends AnyWordSpec with Matchers{

  "CamelCards" should {

    "calculate the example right" in {

      val example = Seq(
          "32T3K 765",
          "T55J5 684",
          "KK677 28",
          "KTJJT 220",
          "QQQJA 483"
      )

      CamelCards.play(example) shouldBe 6440
    }

    "calculate the example with jokers right" in {

      val example = Seq(
        "32T3K 765",
        "T55J5 684",
        "KK677 28",
        "KTJJT 220",
        "QQQJA 483"
      )

      CamelCards.playWithJokers(example) shouldBe 5905
    }
  }

}
