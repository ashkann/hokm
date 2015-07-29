package ir.ashkan.hokm

import scala.Console.println
import scala.collection.SortedMap
import scala.util.Random

object Game extends App {
  implicit val ordering = CardOrderingForConsole
  def printHand(hand: Deck.Hand): String = hand.toList.sorted mkString " "

  class Player(val name: String, val hand: Deck.Hand) {
    override def toString = s"$name ${printHand(hand)}"
  }

  class Team(val player1: Player, val player2: Player) {
    override def toString = s"$player1 and $player2"
  }

  val (h1,h2,h3,h4) = Deck.deal
  val List(p1,p2,p3,p4) = Random.shuffle(List(
    new Player("Jake", h1),
    new Player("Bella", h2),
    new Player("Edward", h3),
    new Player("Ashkan", h4)
  ))
  val team1 = new Team(p1,p2)
  val team2 = new Team(p3,p4)
  val trumpCaller = team1.player1

  println(s"Team 1 : $team1")
  println(s"Team 2 : $team2")
  println(s"Trump-caller is ${trumpCaller.name}")

  println("Call trumps:")
  val trumps = pickCard(trumpCaller, 5).suite
  println(s"Trumps are $trumps")

  private def pickCard(player: Player, howMany: Int = Deck.HandSize): Card = {
    require(howMany >=0 && howMany < Deck.HandSize)
    pickCard(player.hand.toList.take(howMany))
  }

  def pickCard(deck: List[Card]): Card = {
    val menu = SortedMap(('a' to 'z').zip(deck.sorted) :_*)
    println("Pick a card:")
    println(menu map { case (choice, card) => s" $choice $card" } mkString " ")
    println("Which one ? ")
    System.exit(1)
    val choice = scala.io.StdIn.readChar()
    deck(choice)

  }
}
