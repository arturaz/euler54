package net.arturaz

import io.Source

object Log {
  val enabled = false

  def debug(msg: => String) {
    if (enabled) println(msg)
  }
}

object Suite {
  def apply(char: Char) = char match {
    case 'H' => Hearts
    case 'D' => Diamonds
    case 'C' => Crosses
    case 'S' => Spades
  }
}
sealed abstract class Suite(val sortIndex: Int, char: Char) {
  override def toString: String = char.toString
}
case object Hearts extends Suite(0, 'H')
case object Diamonds extends Suite(1, 'D')
case object Crosses extends Suite(2, 'C')
case object Spades extends Suite(3, 'S')

object Card {
  def apply(str: String) = {
    val value = valueOf(str(0))
    val suite = Suite(str(1))
    new Card(suite, value)
  }

  private[this] val values =
    (('2' to '9') ++ Seq('T', 'J', 'Q', 'K', 'A')).zip(Stream.from(2)).toMap
  private val flippedValues = values.map { case (k, v) => v -> k }

  def valueOf(char: Char) = values(char)

  implicit object Ordering extends Ordering[Card] {
    def compare(x: Card, y: Card): Int = {
      val valuesComparison = x.value compare y.value
      if (valuesComparison != 0) valuesComparison
      else x.suite.sortIndex compare y.suite.sortIndex
    }
  }
}
case class Card private (suite: Suite, value: Int) {
  override def toString = s"${Card.flippedValues(value)}$suite"
}

object Hand {
  def apply(cards: Seq[Card]): Hand = {
    require(cards.size == 5, s"Hand must be from 5 cards: $cards")
    new Hand(cards.sorted(Card.Ordering))
  }

  def fromStrings(cards: Seq[String]): Hand = apply(cards.map(Card(_)))
}

/**
 * @param cards Sorted list of cards. Most valuable cards are at the end.
 */
class Hand private (val cards: Seq[Card]) {
  def betterThan(other: Hand) = Combination.compare(this, other) == 1
  lazy val values = cards.map(_.value)
  lazy val byValue = cards.groupBy(_.value)

  override def toString: String = s"(${cards.mkString(" ")})"
}

sealed trait Combination {
  def matches(hand: Hand): List[Card]

  def isSameSuite(hand: Hand) =
    hand.cards.forall(_.suite == hand.cards.head.suite)

  def isConsecutive(hand: Hand) = {
    hand.cards.tail.foldLeft((hand.cards.head, true)) {
      case (t @ (last, bool), next) =>
        if (bool && last.value + 1 == next.value) (next, true)
        else (next, false)
    }._2
  }

}
object Combination {
  object RoyalFlush extends Combination {
    def matches(hand: Hand) = {
      if (isSameSuite(hand) && hand.values == (10 to 14)) hand.cards.toList
      else Nil
    }
  }

  object StraightFlush extends Combination {
    def matches(hand: Hand) =
      if (isSameSuite(hand) && isConsecutive(hand)) hand.cards.toList
      else Nil
  }

  object FourOfAKind extends Combination {
    def matches(hand: Hand) =
      hand.byValue.values.find { cards => cards.size == 4 }.map(_.toList).
        getOrElse(Nil)
  }

  object FullHouse extends Combination {
    def matches(hand: Hand) = {
      val values = hand.byValue.values
      if (
        values.exists { cards => cards.size == 3 } &&
        values.exists { cards => cards.size == 2 }
      ) hand.cards.toList
      else Nil
    }
  }

  object Flush extends Combination {
    def matches(hand: Hand) =
      if (isSameSuite(hand)) hand.cards.toList else Nil
  }

  object Straight extends Combination {
    def matches(hand: Hand) =
      if (isConsecutive(hand)) hand.cards.toList else Nil
  }

  object ThreeOfAKind extends Combination {
    def matches(hand: Hand) =
      hand.byValue.values.find { cards => cards.size >= 3 }.map(_.toList).
        getOrElse(Nil)
  }
  
  object TwoPairs extends Combination {
    def matches(hand: Hand) = {
      val filtered = hand.byValue.values.filter { cards => cards.size >= 2 }
      if (filtered.size >= 2) filtered.toList.flatten.sorted(Card.Ordering)
      else Nil
    }
  }
  
  object OnePair extends Combination {
    def matches(hand: Hand) =
      hand.byValue.values.find { cards => cards.size >= 2 }.map(_.toList).
        getOrElse(Nil)
  }

  object HighCard extends Combination {
    def matches(hand: Hand) = hand.cards.last :: Nil
  }

  val combinations = Seq(
    HighCard, OnePair, TwoPairs, ThreeOfAKind, Straight, Flush, FullHouse,
    FourOfAKind, StraightFlush, RoyalFlush
  ).reverse

  def compare(h1: Hand, h2: Hand): Int = {
    combinations.foreach { combination =>
      (combination matches h1, combination matches h2) match {
        case (cards1 @ Nil, cards2 @ Nil) =>
          Log.debug(s"[$h1 $cards1] ~ [$h2 $cards2] : $combination")
          // None matched, check again
        case (cards1 @ Nil, cards2: List[Card]) =>
          Log.debug(s"[$h1 $cards1] < [$h2 $cards2] : $combination")
          return -1
        case (cards1: List[Card], cards2 @ Nil) =>
          Log.debug(s"[$h1 $cards1] > [$h2 $cards2] : $combination")
          return 1
        case (cards1: List[Card], cards2: List[Card]) =>
          // Both matched, check individual cards
          (cards1 zip cards2).reverse.foreach { case (c1, c2) =>
            val compared = Card.Ordering.compare(c1, c2)
            if (compared != 0) {
              if (compared == -1)
                Log.debug(s"[$h1 $cards1] < [$h2 $cards2] : $combination")
              else
                Log.debug(s"[$h1 $cards1] > [$h2 $cards2] : $combination")
              return compared
            }
          }
      }
    }

    0
  }
}

object Euler54 extends App {
  val input = args(0)
  val source = Source.fromFile(input).getLines()
  val better = source.foldLeft(0) { case (p1Wins, line) =>
    Log.debug(s"Read: $line")
    val (p1Hand, p2Hand) = parseLine(line)
    if (p1Hand betterThan p2Hand) p1Wins + 1 else p1Wins
  }
  println(s"P1 has $better better hands than P2.")

  private[this] def parseLine(line: String) = {
    line.split(" ").grouped(5).toList match {
      case p1 :: p2 :: Nil =>
        (Hand.fromStrings(p1), Hand.fromStrings(p2))
      case o @ _ =>
        throw new IllegalArgumentException(s"Not two hands: $o")
    }
  }
}
