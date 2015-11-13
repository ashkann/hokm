package ir.ashkan.hokm

import ir.ashkan.hokm.Game.Team
import scala.collection.SortedMap
import scala.Console._

abstract class ConsoleInterface {
  import DSL._
  import Game.Player

  implicit def cardOrdering: Ordering[Card]
  var trumps: Suite = _
  var trumpCaller: Player = _
  var trumpCallerTeamMate: Player = _

  def print(card: Card): String = decor(card) + plain(card)
  def print(player: Player): String = decor(player) + plain(player)
  def print(team: Team): String = print(team.player1) + " and " + print(team.player2)

  def pickCard(player: Player, howMany: Int = Deck.HandSize): Card = {
    require(howMany >0 && howMany <= Deck.HandSize)
    pickCard(player.hand.toList.take(howMany),Seq())
  }

  def pickCard(player: Player, lead: Suite): Card = {
    val (valids,invalids) = player.partition(lead)
    pickCard(valids.toSeq,invalids.toSeq)
  }

  // Decorations and Plains
  import ir.ashkan.hokm.Suite.{Hearts,Spades,Diamonds,Clubs}
  private val black = Console.WHITE_B+ Console.BLACK
  private val red = Console.WHITE_B + Console.RED
  private val color = Map( Hearts -> red,  Diamonds -> red, Clubs -> black, Spades -> black)
  private val silver = ""
  private val gold = Console.BOLD + Console.YELLOW
  private val crown = '\u2654'
  private val silverCrown = silver + crown + Console.RESET
  private val goldenCrown = gold + crown + Console.RESET
  private def plain(p: Player) = p.name
  private def plain(c: Card) = color(c.suite) + s"${c.rank}${c.suite}" + Console.RESET
  private def decor(c: Card) =  if (c.suite == trumps) goldenCrown else ""
  private def decor(p: Player) = if (p == trumpCaller)
    goldenCrown
  else if (p == trumpCallerTeamMate)
    silverCrown
  else ""

  private def pickCard(valids: Seq[Card], invalids: Seq[Card]): Card = {
    val menu = SortedMap(('a' to 'z').zip(valids): _*)
    val choices = menu.keySet
    val canPicks = menu map { case (char, card) => s"$char" + print(card) }
    val all = (canPicks ++ invalids.map(print)) mkString " "

    val choice = repeatUntil {
      println(all)
      println("Which one ? ")
      val choice = scala.io.StdIn.readChar()
      if (choice == 'z') System.exit(1)
      choice
    }(choice => choices contains choice)

    menu(choice)
  }
}