package core

import common.State

case class Deck(cards: List[Card])

object Deck {
  val sortedDeck = Deck(Card.cards)

  def shuffle(deck: Deck, random: RandomGenerator) = {
    var currentRandom = random

    val cardsBuffer = deck.cards.toBuffer
    for (i <- 0 until 52) {
      val (nextByte, nextRandom) = currentRandom.generateNextByte

      val indexToSwap = if (nextByte > 51) nextByte >> 3 else nextByte

      val tmp = cardsBuffer(i)
      cardsBuffer(i) = cardsBuffer(indexToSwap)
      cardsBuffer(indexToSwap) = tmp

      currentRandom = nextRandom
    }
    (Deck(cardsBuffer.toList), currentRandom)
  }

  //TODO do I need this?
  type DeckGenerator = State[RandomGenerator, Deck]

  def shuffle(deck: Deck): DeckGenerator = State(shuffle(deck, _))

  def shuffledDeckStream(initialDeck: Deck, random: RandomGenerator): Stream[Deck] = {
    val (nextDeck, nextRandom) = shuffle(initialDeck, random)
    Stream.cons(nextDeck, shuffledDeckStream(nextDeck, nextRandom))
  }
}