package scala.ir.ashkan.hokm

import scala.Console.println
import scala.collection.SortedMap
import scala.util.Random

object Game extends App {
  import DSL._
  import ir.ashkan.hokm.Deck.{Batch, Deck, Hand}

  val Crown = '\u2654'
  val TrumpCallerTeamMateCrown = Crown + Console.RESET
  val TrumpCallerCrown = Console.YELLOW + TrumpCallerTeamMateCrown

  implicit val ordering = CardOrderingForConsole

  class Player(val name: String, val hand: Hand) {
    // @todo Hand is mutable, but partition is immutable
    def partition(leadSuite: Suite): (Batch,Batch) = hand.toSet.partition(_.suite == leadSuite)

    def validCardsToPlay(leadSuite: Suite) = {
      val (followSuites, sluffs) = partition(leadSuite)
      if (followSuites.isEmpty) sluffs else followSuites
    }

    override def toString = this match {
      case `trumpCaller` => TrumpCallerCrown + name
      case `trumpCallerTeamMate` => TrumpCallerTeamMateCrown + name
      case _ => name
    }
  }

  class Team(val player1: Player, val player2: Player) {
    override def toString = s"$player1 and $player2"
  }

  class Trick(val lead: Player) {
    val secondPlayer = nextPlayer(lead)
    val thirdPlayer = nextPlayer(secondPlayer)
    val fourthPlayer = nextPlayer(thirdPlayer)
    var leadCard, card2, card3, card4 : Card = _

    def nextPlayer(current: Player) = current match {
      case team1.player1 => team2.player1
      case team2.player1 => team1.player2
      case team1.player2 => team2.player2
      case team2.player2 => team1.player1
    }

    override def toString = {
      val plays = Map(lead->leadCard, secondPlayer->card2, thirdPlayer->card3, fourthPlayer->card4)
      plays map { case (player,card)=>s"$player: $card" } mkString(",")
    }
  }

  val (h1,h2,h3,h4) = Deck.deal
  val List(p1,p2,p3,p4) = Random.shuffle(List(
    new Player("Jake", h1),
    new Player("Bella", h2),
    new Player("Edward", h3),
    new Player("Ashkan", h4)
  ))
  val team1 = new Team(p1,p3)
  val team2 = new Team(p2,p4)
  val trumpCaller = team1.player1
  val trumpCallerTeamMate = team1.player2

  println(s"$team1 vs $team2")

  println(s"$trumpCaller, call trumps:")
  val trumps = pickCard(trumpCaller, 5).suite
  Card.trumps = trumps

  val trick = new Trick(trumpCaller)
  playTrick(trick)
  println(trick)

  def playTrick(trick: Trick) {
    println(s"${trick.lead}, you are the trick-leader. Play a card")
    val lead = pickCard(trick.lead)
    trick.leadCard = lead
    println(s"${trick.lead} played $lead")

    println(s"${trick.secondPlayer}, play a card")
    val card2 = pickCard(trick.secondPlayer,lead.suite)
    trick.card2 = card2
    println(s"{${trick.secondPlayer} played $card2")

    println(s"${trick.thirdPlayer}, play a card")
    val card3 = pickCard(trick.thirdPlayer,lead.suite)
    trick.card3 = card3
    println(s"${trick.thirdPlayer} played $card3")

    println(s"${trick.fourthPlayer}, play a card")
    val card4 = pickCard(trick.fourthPlayer,lead.suite)
    trick.card4 = card4
    println(s"${trick.fourthPlayer} played $card4")
  }

  def pickCard(player: Player, howMany: Int = Deck.HandSize): Card = {
    require(howMany >0 && howMany <= Deck.HandSize)
    pickCard(player.hand.toList.take(howMany))
  }

  def pickCard(player: Player, leadSuite: Suite): Card = {
    val (valids,invalids) = player.partition(leadSuite)
    pickCard(valids,invalids)
  }

  def pickCard(valids: Batch, invalids: Batch): Card = {
    val menu = SortedMap(('a' to 'z').zip(valids.toList.sorted) :_*)
    val validChoices = menu.keys.toSet

    val choice = repeatUntil {
      val canPicks = menu map { case (choice, card) => s"$choice $card" } mkString " "
      val cantPicks = invalids.toList.sorted.mkString(" ")
      println(canPicks + " " + cantPicks)
      println("Which one ? ")
      scala.io.StdIn.readChar()
    } (choice => validChoices contains choice)

    menu(choice)
  }

  def pickCard(cards: Deck): Card = {
    val menu = SortedMap(('a' to 'z').zip(cards.sorted) :_*)
    val validChoices = menu.keys.toSet

    val choice = repeatUntil {
      println(menu map { case (choice, card) => s"$choice $card" } mkString " ")
      println("Which one ? ")
      scala.io.StdIn.readChar()
    } (choice => validChoices contains choice)

    menu(choice)
  }
}

object DSL {
  /**
   * Repeatedly performs a piece of code and checks the result until a condition holds
   * @param code The piece of code to execute
   * @param condition The condition to observe
   * @tparam T Type of the result that code returns
   * @return The result for the last execution of code for which the condition was True
   */
  def repeatUntil[T](code: =>T)(condition: T => Boolean): T = {
    code match {
      case result if condition(result) => result
      case _ => repeatUntil(code)(condition)
    }
  }
}

object Semantics {

}
