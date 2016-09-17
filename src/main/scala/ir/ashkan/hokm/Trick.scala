package ir.ashkan.hokm

import ir.ashkan.hokm.Deck.Deck

class Trick(val lead: Player, val team1: Team, val team2: Team, ordering: CardOrdering) {
//  type P = team1.P

  private var leadCard, card2, card3, card4 : Card = _

//  private val second2: Player = next(new Player("invalid",Deck.emptyHand))
  private val second: Player = next(lead)
  private val third:  Player = next(second)
  private val fourth: Player = next(third)
  val players: Seq[Player] = Seq(lead,second,third,fourth)
  val others: Seq[Player] = players.tail

  lazy val leadSuite: Suite = leadCard.suite
  def taker: Player = playerOf(topCard)
  def takerTeam: Team = teamOf(taker)
  def topCard: Card = cards.max(ordering)
  def plays: Map[Player,Card] = Map(
    lead->leadCard,
    second->card2,
    third->card3,
    fourth->card4) filter { case (_,card) => card != null }

  def update(player: Player, card: Card): Unit = player match {
    case `lead`   => leadCard = card
    case `second` => card2 = card
    case `third`  => card3 = card
    case `fourth` => card4 = card
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

  private def teamOf(player: Player): Team = if(player playsIn team1) team1 else team2

  private def next(current: Player) = {
    val table : Seq[Player] = Seq(team1.player1,team2.player1,team1.player2,team2.player2)
    val currentPosition = table.indexOf(current)
    table(if (currentPosition == 3) 0 else currentPosition + 1)
  }
}