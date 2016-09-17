package ir.ashkan.hokm

case class Player(name: String) {
  def playsIn(team: Team) = team contains this
}

import ir.ashkan.hokm.Deck._

class InGamePlayer(val player: Player, val hand: Hand) {
  // @todo Hand is mutable, but partition is immutable
  def partition(leadSuite: Suite): (Batch,Batch) = hand.toSet.partition(_.suite == leadSuite)

  def validCardsToPlay(leadSuite: Suite) = {
    val (followSuites, sluffs) = partition(leadSuite)
    if (followSuites.isEmpty) sluffs else followSuites
  }

}