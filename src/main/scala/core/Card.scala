package core

import core.Rank.Rank
import core.Suit.Suit

case class Card(rank: Rank, suit: Suit) {

  override def toString = {
    val rankChar = rank match {
      case Rank.Ace => 'A'
      case Rank.Two => '2'
      case Rank.Three => '3'
      case Rank.Four => '4'
      case Rank.Five => '5'
      case Rank.Six => '6'
      case Rank.Seven => '7'
      case Rank.Eight => '8'
      case Rank.Nine => '9'
      case Rank.Ten => 'T'
      case Rank.Jack => 'J'
      case Rank.Queen => 'Q'
      case Rank.King => 'K'
    }
    val suitChar = suit match {
      case Suit.Spade => 's'
      case Suit.Heart => 'h'
      case Suit.Diamond => 'd'
      case Suit.Club => 'c'
    }
    "%c%c" format(rankChar, suitChar)
  }
}

object Card {
  val cards = for {
    suit <- Suit.values.toList
    rank <- Rank.values.toList
  } yield Card(rank, suit)
}

object Rank extends Enumeration {
  type Rank = Value
  val Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King = Value
}

object Suit extends Enumeration {
  type Suit = Value
  val Spade, Heart, Diamond, Club = Value
}