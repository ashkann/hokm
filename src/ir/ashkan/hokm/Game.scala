package ir.ashkan.hokm

import ir.ashkan.hokm.Deck.{Deck, Hand}

import Console.println
import scala.collection.SortedMap
import scala.util.Random

object Game extends App {
  import DSL._

  implicit val ordering = CardOrderingForConsole

  class Player(val name: String, val hand: Hand) {
    override def toString = this match {
      case `trumpCaller` => Console.YELLOW + s"\u2654" + Console.RESET + name
      case `trumpCallerTeamMate` => s"\u2654" + name
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

    def nextPlayer(current: Player) = current match {
      case team1.player1 => team2.player1
      case team2.player1 => team1.player2
      case team1.player2 => team2.player2
      case team2.player2 => team1.player1
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
  println(s"Trumps are $trumps")

  val trick = new Trick(trumpCaller)
  playTrick(trick)
  println(trick)

  def playTrick(trick: Trick): (Card,Card,Card,Card) = {
    val lead = playCard(trick.lead, "you are the trick-leader. Play a card")
    val card2 = playCard(trick.secondPlayer, "it's your turn. Play a card")
    val card3 = playCard(trick.thirdPlayer, "it's your turn. Play a card")
    val card4 = playCard(trick.fourthPlayer, "it's your turn. Play a card")
    (lead, card2, card3, card4)
  }

  def playCard(player: Player , msg: String): Card = {
    println(s"$player, $msg")
    val card = pickCard(player)
    player.hand.remove(card)
    println(s"$player played $card")
    card
  }

  def pickCard(player: Player, howMany: Int = Deck.HandSize): Card = {
    require(howMany >0 && howMany <= Deck.HandSize)
    pickCard(player.hand.toList.take(howMany))
  }

  def pickCard(deck: Deck): Card = {
    val menu = SortedMap(('a' to 'z').zip(deck.sorted) :_*)
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
