package ir.ashkan.hokm

import scala.collection.SortedMap
import scala.Console._
import DSL._

/**
  * Provides input/output via terminal (console)
  */
abstract class Terminal {
  val cardOrdering: CardOrdering
  val goldPlayer: Player
  val silverPlayer: Player
  var trumps: Suite = _

  def apply(card: Card) = print(card)
  def apply(player: Player) = print(player)
  def apply(team: Team) = print(team)
  def apply(trick: Trick) = print(trick)

  def print(card: Card): String = decor(card) + plain(card)
  def print(player: Player): String = decor(player) + plain(player)
  def print(team: Team): String = "(" + print(team.player1) + "," + print(team.player2) + ")"
  def print(trick: Trick): String = trick.plays map { case (player,card)=>
    (if(player == trick.taker) winnerDecoration else noDecoration) + print(player) + print(card) + end
  } mkString space

  def pick(player: Player, howMany: Int): Card = {
    require(howMany >0 && howMany <= Deck.HandSize)
    pick(player.hand.toList.take(howMany),Seq())
  }

  def pick(player: Player, lead: Suite): Card = {
    val (valids,invalids) = player.partition(lead)
    pick(valids.toSeq,invalids.toSeq)
  }

  // Decorations and Plains
  private val end = Console.RESET
  private val winnerDecoration = Console.BOLD
  private val noDecoration = ""
  private val space = " "
  import ir.ashkan.hokm.Suite.{Hearts,Spades,Diamonds,Clubs}
  private val black = Console.WHITE_B+ Console.BLACK
  private val red = Console.WHITE_B + Console.RED
  private val color = Map( Hearts -> red,  Diamonds -> red, Clubs -> black, Spades -> black)
  private val silver = ""
  private val gold = Console.BOLD + Console.YELLOW
  private val crown = '\u2654'
  private val silverCrown = silver + crown + end
  private val goldenCrown = gold + crown + end
  private def plain(p: Player) = p.name
  private def plain(c: Card) = color(c.suite) + s"${c.rank}${c.suite}" + end
  private def decor(c: Card) =  if (c.suite == trumps) goldenCrown else noDecoration
  private def decor(p: Player) = if(p == goldPlayer) goldenCrown else if(p == silverPlayer) silverCrown else noDecoration

  private def pick(valids: Seq[Card], invalids: Seq[Card])(implicit ordering:CardOrdering = cardOrdering): Card = {
    val menu = SortedMap(('a' to 'z').zip(valids.sorted): _*)
    val choices = menu.keySet
    val canPicks = menu map { case (char, card) => s"$char" + print(card) }
    val all = (canPicks ++ invalids.sorted.map(print)) mkString " "

    val choice = repeatUntil[Char] {
      println(all)
      println("Which one ? ")
      val choice = scala.io.StdIn.readChar()
      if (choice == 'z') System.exit(1)
      choice
    }(choice => choices contains choice)

    menu(choice)
  }
}