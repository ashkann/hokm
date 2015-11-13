package ir.ashkan.hokm

import ir.ashkan.hokm.Suite._
import scala.Console.println
import scala.collection.mutable
import scala.util.Random
import DSL._
import ir.ashkan.hokm.Deck.{Batch, Deck, Hand}

object Game extends App { gameInProgress =>

  implicit val ordering = CardOrdering(SuiteOrdering(Hearts, Spades, Diamonds, Clubs), RankOrdering.natural)

//  implicit class ConsoleContext(private val sc:StringContext) extends {
//    def hokm(args: Any*): String = {
//      val vs = args map {
//        case team:Team => "Team"
//        case _ => "Dunno!"
//      }
//
//      (vs mkString ",") + (sc.parts mkString "|")
//    }
//  }

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
  interface.trumpCaller = trumpCaller
  interface.trumpCallerTeamMate = team1.player2

  println(interface.print(team1) + " vs " + interface.print(team2))
  println(interface.print(trumpCaller) + " call trumps:")

  val trumps = interface.pickCard(trumpCaller, 5).suite
  interface.trumps = trumps

  var lead = trumpCaller
  repeatUntil[Team] {
    val trick = new Trick(lead)
    playTrick(trick)
    lead = trick.winner

    val winnerTeam = team(lead)
    winnerTeam.score += 1
    println(interface.print(winnerTeam) + " took the deal")

    winnerTeam
  } (winner => winner.score >= 7)


  class Player(val name: String, val hand: Hand) {
    // @todo Hand is mutable, but partition is immutable
    def partition(leadSuite: Suite): (Batch,Batch) = hand.toSet.partition(_.suite == leadSuite)

    def validCardsToPlay(leadSuite: Suite) = {
      val (followSuites, sluffs) = partition(leadSuite)
      if (followSuites.isEmpty) sluffs else followSuites
    }

    def playsIn(team: Team) = team contains this
  }

  class Team(val player1: Player, val player2: Player) {
    var score: Int = 0
    def contains(player: Player): Boolean = player == player1 || player == player2
  }

  class Trick(val lead: Player) {
    var leadCard, card2, card3, card4 : Card = _

    val second: Player = nextPlayer(lead)
    val third:  Player = nextPlayer(second)
    val fourth: Player = nextPlayer(third)
    lazy val leadSuite: Suite = leadCard.suite
    def winner: Player = playerOf(topCard)
    def topCard: Card = cards.max

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

    override def toString = {
      val plays = Map(lead->leadCard,second->card2,third->card3,fourth->card4) filter { case (_,card) => card != null }
      plays map { case (player,card)=>
        if(player == winner)
          Console.BOLD+s"$player " + interface.print(card) + Console.RESET
        else
          s"$player" + interface.print(card) } mkString "  "
    }
  }

  def team(player: Player): Team = if(player playsIn team1) team1 else team2

  def playTrick(trick: Trick) {
    println(interface.print(trick.lead) + ", you are the trick-leader. Play a card")
    trick(1) = interface.pickCard(trick.lead)
    println(trick)

    for(turn <- 2 to 4) {
      println(interface.print(trick.player(turn)) + ", play a card")
      trick(turn) = interface.pickCard(trick.player(turn),trick.leadSuite)
      println(trick)
    }
  }

  def deal: (Hand,Hand,Hand,Hand) = {
    val List(h1,h2,h3,h4) = Deck.deck.grouped(Deck.HandSize).toList
    (h1,h2,h3,h4)
  }
  implicit def toHand(cards:List[Card]): Hand = mutable.Set(cards: _*)
}