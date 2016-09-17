package ir.ashkan.hokm

import scala.collection.mutable
import scala.util.Random
import ir.ashkan.hokm.Deck.Hand

class Table
(
  val team1: Team,
  val team2: Team
) {
  private[this] val (h1,h2,h3,h4) = Table.deal
  val ph = Map[Player,Hand](
    team1.player1 -> h1,
    team1.player2 -> h2,
    team2.player1 -> h3,
    team2.player2 -> h4
  )
}

object Table {
  def deal: (Hand,Hand,Hand,Hand) = {
    def toHand(cards:List[Card]): Hand = mutable.Set(cards: _*)

    val List(h1,h2,h3,h4) = deck.grouped(Deck.HandSize).toList
    (toHand(h1),toHand(h2),toHand(h3),toHand(h4))
  }

  private def deck: Deck.Deck = Random.shuffle(Deck.allCards.toList)
}
