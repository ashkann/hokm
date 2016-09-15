package ir.ashkan.hokm

import ir.ashkan.hokm.DSL._
import ir.ashkan.layout.Element

import scala.Console._
import scala.collection.SortedMap

trait Card2D {
  val space = " "

  def print(cards: Seq[ Card ]): Element = cards.map(print).foldRight(Element.Nil) { _ beside _ }

  def print(card: Card): Element = {
    val padding = if (card.rank == Rank._10) space else space * 2
    val s = card.suite.symbol
    val r = card.rank.name
    Element.round(
      Element(s + padding + r) above
        Element(space) above
        Element(r + padding + s)
    )
  }
}

trait Player2D {
  type Decor = Element=>Element

  val goldPlayer: Player
  val silverPlayer: Player

  private val goldPlayerDecorator: Decor = Element.double
  private val silverPlayerDecorator: Decor = Element.single
  private val normalPlayerDecorator: Decor = Element.round

  def print(player: Player): Element = decor(player)(plain(player))

  private def plain(player: Player): Element = Element(player.name)

  private def decor(player: Player): Decor = player match  {
    case `goldPlayer` => goldPlayerDecorator
    case `silverPlayer` => silverPlayerDecorator
    case _ => normalPlayerDecorator
  }
}

trait Menu2D {
  val cardOrdering: CardOrdering
  def print(c:Card):Element
//  def print(cs:Seq[Card]):Element

  def pick(player: Player, howMany: Int): Card = {
    require(howMany > 0 && howMany <= Deck.HandSize)
    pick(player.hand.toList.take(howMany), Seq())
  }

  def pick(player: Player, lead: Suite): Card = {
    val (valids, invalids) = player.partition(lead)
    pick(valids.toSeq, invalids.toSeq)
  }

  private def pick(valids: Seq[ Card ], invalids: Seq[ Card ]): Card = {
    val menu = SortedMap(('a' to 'z').zip(valids): _*)
    val choices = menu.keySet
    val canPicks = menu map { case (char, card) => print(card) above Element.single(Element(char)) }
    val all = Element.beside((canPicks ++ invalids.map(print)).toSeq)

    val choice = repeatUntil[ Char ] {
      println(all)
      println("Which one ? ")
      val choice = scala.io.StdIn.readChar()
      if (choice == 'z') System.exit(1)
      choice
    }(choice => choices contains choice)

    menu(choice)
  }
}

/**
  * Provides input/output via terminal (console)
  */
abstract class Terminal2D extends Card2D with Player2D with Menu2D {
  var trumps: Suite = _

  def apply(card: Card) = print(card)
  def apply(cards: Seq[ Card ]) = print(cards)
  def apply(player: Player) = print(player)
  def apply(team: Team) = print(team)
  def apply(trick: Trick) = print(trick)

  def print(team: Team): Element = this(team.player1) beside this(team.player2)

  def print(trick: Trick): String = trick.plays map { case (player, card) =>
    (if (player == trick.taker) winnerDecoration else noDecoration) + print(player) + print(card) + end
  } mkString space

  // Decorations and Plains
  private val end = Console.RESET
  private val winnerDecoration = Console.BOLD
  private val noDecoration = ""
  private val black = Console.WHITE_B + Console.BLACK
  private val red = Console.WHITE_B + Console.RED

  private val silver = ""
  private val gold = Console.BOLD + Console.YELLOW
  private val crown = '\u2654'
  private val silverCrown = silver + crown + end
  private val goldenCrown = gold + crown + end
}