package ir.ashkan.hokm

import scala.collection.mutable
import scala.util.Random
import ir.ashkan.hokm.Deck.{ Hand => H }

case class Player(name: String) {
  def playsIn(team: Team) = team contains this
}

case class Team(player1: Player, player2: Player) {
  def contains(player: Player): Boolean = player == player1 || player == player2
}

class Table
(
  val team1: Team,
  val team2: Team
) {
  private[this] val (h1,h2,h3,h4) = Table.deal
  private[this] val players = Seq(team1.player1,team2.player1,team1.player2,team2.player2)
  private val hands = Map[Player,H](
    team1.player1 -> h1,
    team1.player2 -> h2,
    team2.player1 -> h3,
    team2.player2 -> h4
  )

  def hand(who:Player):H= hands(who)

  def turn(lead:Player): Seq[Player] = turn(players.indexOf(lead))
  private def turn(from:Int): Seq[Player] = (0 to 3).map { i => players(from + i % 4) }
}


object Table {
  def deal: (H,H,H,H) = {
    def toHand(cards:List[Card]): H = mutable.Set(cards: _*)

    val List(h1,h2,h3,h4) = deck.grouped(Deck.HandSize).toList
    (toHand(h1),toHand(h2),toHand(h3),toHand(h4))
  }

  private def deck: Deck.Deck = Random.shuffle(Deck.allCards.toList)
}
