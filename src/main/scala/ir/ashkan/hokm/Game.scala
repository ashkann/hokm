package ir.ashkan.hokm

import ir.ashkan.hokm.Suite._
import scala.Console.println
import scala.collection.mutable
import scala.util.Random
import DSL._
import ir.ashkan.hokm.Deck.Hand

object Game extends App { gameInProgress =>

  implicit val ordering = CardOrdering(SuiteOrdering(Hearts, Spades, Diamonds, Clubs), RankOrdering.natural)

  val (h1,h2,h3,h4) = deal
  val List(p1,p2,p3,p4) = Random.shuffle(List(
    new Player("Jake", h1),
    new Player("Bella", h2),
    new Player("Edward", h3),
    new Player("Ashkan", h4)
  ))
  val team1 = new Team(p1,p3)
  val team2 = new Team(p2,p4)
  val trumpCaller = team1.player1

  val interface = new ConsoleInterface {
    val cardOrdering = ordering
  }
  interface.goldPlayer = trumpCaller
  interface.silverPlayer = team1.player2

  println(interface.print(team1) + " vs " + interface.print(team2))
  println(interface.print(trumpCaller) + " call trumps:")

  val trumps = interface.pickCard(trumpCaller, 5).suite
  interface.trumps = trumps

  var lead = trumpCaller
  repeatUntil[Team] {
    val trick = new Trick(lead,team1,team2,ordering)
    playTrick(trick)
    lead = trick.winner

    val winnerTeam = team(lead)
    winnerTeam.score += 1
    println(interface.print(winnerTeam) + " took the deal")

    winnerTeam
  } (winner => winner.score >= 7)


  def team(player: Player): Team = if(player playsIn team1) team1 else team2

  def playTrick(trick: Trick) {
    println(interface.print(trick.lead) + ", you are the trick-leader. Play a card")
    trick(1) = interface.pickCard(trick.lead)
    println(interface.print(trick))

    for(turn <- 2 to 4) {
      println(interface.print(trick.player(turn)) + ", play a card")
      trick(turn) = interface.pickCard(trick.player(turn),trick.leadSuite)
      println(interface.print(trick))
    }
  }

  def deal: (Hand,Hand,Hand,Hand) = {
    val List(h1,h2,h3,h4) = Deck.deck.grouped(Deck.HandSize).toList
    (h1,h2,h3,h4)
  }
  implicit def toHand(cards:List[Card]): Hand = mutable.Set(cards: _*)
}
