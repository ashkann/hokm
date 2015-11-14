package ir.ashkan.hokm

import ir.ashkan.hokm.Deck.Deck

class Trick(val lead: Player, team1: Team, team2: Team, implicit private val ordering: CardOrdering) {
  private var leadCard, card2, card3, card4 : Card = _

  private val second: Player = nextPlayer(lead)
  private val third:  Player = nextPlayer(second)
  private val fourth: Player = nextPlayer(third)

  lazy val leadSuite: Suite = leadCard.suite
  def winner: Player = playerOf(topCard)
  def topCard: Card = cards.max
  def plays: Map[Player,Card] = Map(lead->leadCard,second->card2,third->card3,fourth->card4) filter { case (_,card) => card != null }

  def update(index: Int, card: Card): Unit = index match {
    case 1 => leadCard = card
    case 2 => card2 = card
    case 3 => card3 = card
    case 4 => card4 = card
  }

  def apply(index: Int): Card = index match {
    case 1 => leadCard
    case 2 => card2
    case 3 => card3
    case 4 => card4
  }

  def player(index: Int): Player = index match {
    case 1 => lead
    case 2 => second
    case 3 => third
    case 4 => fourth
  }

  private def cards: Deck = List(leadCard, card2, card3, card4) filter { _ != null }

  private def playerOf(card: Card): Player = Map(
    leadCard->lead,
    card2->second,
    card3->third,
    card4->fourth
  )(card)

  private def nextPlayer(current: Player) = current match {
    case team1.player1 => team2.player1
    case team2.player1 => team1.player2
    case team1.player2 => team2.player2
    case team2.player2 => team1.player1
  }
}