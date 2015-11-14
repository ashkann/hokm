package ir.ashkan.hokm

import ir.ashkan.hokm.Deck._

class Player(val name: String, val hand: Hand) {
  // @todo Hand is mutable, but partition is immutable
  def partition(leadSuite: Suite): (Batch,Batch) = hand.toSet.partition(_.suite == leadSuite)

  def validCardsToPlay(leadSuite: Suite) = {
    val (followSuites, sluffs) = partition(leadSuite)
    if (followSuites.isEmpty) sluffs else followSuites
  }

  def playsIn(team: Team) = team contains this
}