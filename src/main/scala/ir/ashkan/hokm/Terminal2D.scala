package ir.ashkan.hokm

import ir.ashkan.hokm.DSL._
import ir.ashkan.layout.Element

import scala.Console._
import scala.collection.SortedMap

/**
  * Provides input/output via terminal (console)
  */
abstract class Terminal2D {
  type Decor = Element=>Element

  val cardOrdering: CardOrdering
  val goldPlayer: Player
  val silverPlayer: Player
  var trumps: Suite = _

  private val goldPlayerDecorator: Decor = Element.double
  private val silverPlayerDecorator: Decor = Element.single
  private val normalPlayerDecorator: Decor = Element.round


  def apply(card: Card) = print(card)

  def apply(player: Player) = print(player)

  def apply(team: Team) = print(team)

  def apply(trick: Trick) = print(trick)

  def apply(cards: Seq[ Card ]) = print(cards)

  def print(cards: Seq[ Card ]): Element = cards.map(print).foldRight(Element.Nil) { _ besides _ }

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

  def print(player: Player): Element = decor(player)(plain(player))

  private def plain(player: Player): Element = Element(player.name)

  private def decor(player: Player): Decor = player match  {
    case `goldPlayer` => goldPlayerDecorator
    case `silverPlayer` => silverPlayerDecorator
    case _ => normalPlayerDecorator
  }

  def print(team: Team): Element = this(team.player1) besides this(team.player2)

  def print(trick: Trick): String = trick.plays map { case (player, card) =>
    (if (player == trick.taker) winnerDecoration else noDecoration) + print(player) + print(card) + end
  } mkString space


  def pick(player: Player, howMany: Int): Card = {
    require(howMany > 0 && howMany <= Deck.HandSize)
    pick(player.hand.toList.take(howMany), Seq())
  }

  def pick(player: Player, lead: Suite): Card = {
    val (valids, invalids) = player.partition(lead)
    pick(valids.toSeq, invalids.toSeq)
  }

  // Decorations and Plains
  private val end = Console.RESET
  private val winnerDecoration = Console.BOLD
  private val noDecoration = ""
  private val space = " "
  private val black = Console.WHITE_B + Console.BLACK
  private val red = Console.WHITE_B + Console.RED

  private val silver = ""
  private val gold = Console.BOLD + Console.YELLOW
  private val crown = '\u2654'
  private val silverCrown = silver + crown + end
  private val goldenCrown = gold + crown + end

  private def pick(valids: Seq[ Card ], invalids: Seq[ Card ])(implicit ordering: CardOrdering = cardOrdering): Card = {
    val menu = SortedMap(('a' to 'z').zip(valids.sorted): _*)
    val choices = menu.keySet
    val canPicks = menu map { case (char, card) => s"$char" + print(card) }
    val all = (canPicks ++ invalids.sorted.map(print)) mkString " "

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