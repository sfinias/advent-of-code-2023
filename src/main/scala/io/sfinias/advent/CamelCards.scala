package io.sfinias.advent

import scala.io.Source

object CamelCards {

  case class Hand(cards: String, power: Int, bet: Int, compareValue: String) extends Ordered[Hand] {
    override def compare(that: Hand): Int =
      if (power != that.power) power - that.power else compareValue.compareTo(that.compareValue)
  }

  private object Hand {
    def apply(input: String): Hand =
      "(\\w+) (\\d+)".r.findFirstMatchIn(input).map { matched =>
        val cards = matched.group(1)
        val compareValue = cards
          .replace('T', ('9' + 1).toChar)
          .replace('J', ('9' + 2).toChar)
          .replace('Q', ('9' + 3).toChar)
          .replace('K', ('9' + 4).toChar)
          .replace('A', ('9' + 5).toChar)
        val map = cards.groupMapReduce(identity)(_ => 1)(_ + _)
        val power = map.size match {
          case 1 => 7
          case 2 if map.values.exists(_ == 4) => 6
          case 2 => 5
          case 3 if map.values.exists(_ == 3) => 4
          case 3 if map.values.exists(_ == 2) => 3
          case 4 => 2
          case 5 => 1
        }
        val bet = matched.group(2).toInt
        Hand(cards, power, bet, compareValue)
      }.get

    def withJoker(input: String): Hand =
      "(\\w+) (\\d+)".r.findFirstMatchIn(input).map { matched =>
        val cards = matched.group(1)
        val compareValue = cards
          .replace('J', '1')
          .replace('T', ('9' + 1).toChar)
          .replace('Q', ('9' + 2).toChar)
          .replace('K', ('9' + 3).toChar)
          .replace('A', ('9' + 4).toChar)
        val map = cards.groupMapReduce(identity)(_ => 1)(_ + _)
        val jokers = map.getOrElse('J', 0)
        val power = (map - 'J').size match {
          case 1 => 7
          case 2 => jokers match {
            case 3 => 6
            case 2 => 6
            case 1 if map.values.exists(_ == 3) => 6
            case 1 => 5
            case 0 if map.values.exists(_ == 4) => 6
            case _ => 5
          }
          case 3 => jokers match {
            case it if 1 to 2 contains it => 4
            case _ if map.values.exists(_ == 3) => 4
            case _ => 3
          }
          case 4 => 2
          case 5 => 1
          case 0 => 7
        }
        val bet = matched.group(2).toInt
        Hand(cards, power, bet, compareValue)
      }.get

  }
  def play(input: Seq[String]): Int =
    input.map(Hand(_)).sorted.reverse
      .foldLeft(0, input.size) { case ((acc, rank), hand) =>
        (acc + rank * hand.bet, rank - 1)
      }._1

  def playWithJokers(input: Seq[String]): Int =
    input.map(Hand.withJoker).sorted.reverse
      .foldLeft(0, input.size) { case ((acc, rank), hand) =>
        (acc + rank * hand.bet, rank - 1)
      }._1


  def main(args: Array[String]): Unit =
    println(playWithJokers(Source.fromResource("camel-cards.txt").getLines().toSeq))


}


