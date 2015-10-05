package ir.ashkan.hokm

import scala.collection.mutable
import scala.util.Random

object Deck {
  type Hand = mutable.Set[Card]
  type Batch = Set[Card]
  type Deck = List[Card]

  val HandSize = 13
  val DeckSize = 52

  val hearts   = Hearts.cards
  val spades   = Spades.cards
  val clubs    = Clubs.cards
  val diamonds = Diamonds.cards

  val allCards: Batch = hearts ++ clubs ++ spades ++ diamonds
  def deck: Deck = Random.shuffle(Deck.allCards.toList)
  def deal: (Hand,Hand,Hand,Hand) = {
    val List(h1,h2,h3,h4) = deck.grouped(Deck.HandSize).toList
    (h1,h2,h3,h4)
  }

  implicit def toHand(cards:List[Card]): Hand = mutable.Set(cards: _*)
}