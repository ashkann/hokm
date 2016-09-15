package ir.ashkan.hokm

import ir.ashkan.hokm.DSL._
import ir.ashkan.layout.{Element, Aligner}

import scala.Console._
import scala.collection.SortedMap

/**
  * Provides input/output via terminal (console)
  */
abstract class Terminal2D {
  val cardOrdering: CardOrdering
  val goldPlayer: Player
  val silverPlayer: Player
  var trumps: Suite = _

  def apply(card: Card) = print(card)
  def apply(player: Player) = print(player)
  def apply(team: Team) = print(team)
  def apply(trick: Trick) = print(trick)
  def apply(cards: Seq[Card]) = print(cards)

  def print(card: Card): String = decor(card) + plain(card)
  def print(player: Player): String = decor(player) + plain(player)
  def print(team: Team): String = "(" + print(team.player1) + "," + print(team.player2) + ")"
  def print(trick: Trick): String = trick.plays map { case (player,card)=>
    (if(player == trick.taker) winnerDecoration else noDecoration) + print(player) + print(card) + end
  } mkString space
  def print(cards: Seq[Card]): Element = cards.map(element).foldRight(Element.Nil){ _ besides _ }


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
  private val black = Console.WHITE_B+ Console.BLACK
  private val red = Console.WHITE_B + Console.RED
  import ir.ashkan.hokm.Suite.{Clubs, Diamonds, Hearts, Spades}
  private val color = Map( Hearts -> red,  Diamonds -> red, Clubs -> black, Spades -> black)
  private val silver = ""
  private val gold = Console.BOLD + Console.YELLOW
  private val crown = '\u2654'
  private val silverCrown = silver + crown + end
  private val goldenCrown = gold + crown + end
  private def plain(p: Player) = p.name
  private def plain(c: Card) = element(c).toString

  def element(c: Card) = {
    val padding = if (c.rank == Rank._10) space else space*2
    val s = c.suite.symbol
    val r = c.rank.name
    Element.round(
      Element(s + padding + r) above
      Element(space) above
      Element(r + padding + s)
    )
  }

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