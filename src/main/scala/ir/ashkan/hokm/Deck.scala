package ir.ashkan.hokm

import scala.collection.mutable
import scala.util.Random
import ir.ashkan.hokm.Suite.{Hearts,Spades,Diamonds,Clubs}

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
}