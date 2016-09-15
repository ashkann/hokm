package ir.ashkan.hokm

import scala.Console.println
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import DSL._

object Game extends App {

  val (team1,team2) = deal
  val trumpCaller = team1.player1

  val ui = new Terminal2D {
    val cardOrdering = CardOrdering.natural
    val goldPlayer = trumpCaller
    val silverPlayer = team1.player2
  }

  val trumps = callTrumps
  ui.trumps = trumps

  val round = playHand(trumpCaller)
  println(ui(round.lastWinner) + " took the round")

  def callTrumps: Suite = {
    println(ui(team1) + " vs " + ui(team2))
    println(ui(trumpCaller) + " call trumps:")

    ui.pick(trumpCaller, 5).suite
  }

  def playHand(eldest: Player): Hand = {
    val hand = new Hand(team1,team2)

    repeatUntil2[Hand,Player] { lead =>
      val trick = playTrick(lead)

      hand.trickFinished(trick.takerTeam)
      println(ui(trick.takerTeam) + " took the trick")

      println(ui(hand.team1) + ":" + hand.score(team1))
      println(ui(hand.team2) + ":" + hand.score(team2))

      (hand,trick.taker)
    }(_.lastWinnerScore >= 1)(eldest)
  }

  def playTrick(lead: Player): Trick = {
    val trick = new Trick(lead, team1, team2, CardOrdering(trumps))

    println(ui(lead) + ", lead")
    trick(lead) = ui.pick(lead, Deck.HandSize)
    println(ui(trick))

    for (player <- trick.others) {
      println(ui(player) + ", play a card")
      trick(player) = ui.pick(player, trick.leadSuite)
      println(ui(trick))
    }
    trick
  }

  class Hand(val team1: Team, val team2: Team) {
    val tricks = List()
    private val hands = ArrayBuffer[Team]()

    def score(t: Team): Int = hands.count(_ == t)
    def trickFinished(winner: Team) { hands += winner }
    def lastWinner = hands.last
    def lastWinnerScore = score(lastWinner)
  }

  private def deal: (Team,Team) = {
    import ir.ashkan.hokm.Deck.{Hand => H}

    def toHand(cards:List[Card]): H = mutable.Set(cards: _*)

    val List(h1,h2,h3,h4) = Deck.deck.grouped(Deck.HandSize).toList

    val List(p1,p2,p3,p4) = Random.shuffle(List(
      new Player("Jake", toHand(h1)),
      new Player("Bella", toHand(h2)),
      new Player("Edward", toHand(h3)),
      new Player("Ashkan", toHand(h4))
    ))

    (new Team(p1,p3),new Team(p2,p4))
  }
}