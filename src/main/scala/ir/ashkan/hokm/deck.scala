package ir.ashkan.hokm

import scala.collection.mutable
import scala.util.Random

sealed abstract class Suite(val symbol:Char) {
 override def toString = s"$symbol"
}

object Hearts extends Suite('\u2665')
object Spades extends Suite('\u2660')
object Clubs extends Suite('\u2663')
object Diamonds extends Suite('\u2666')

sealed abstract class Rank(val rank: Int, val name:String) {
  def this(rank: Int) = this(rank,rank.toString)
  override def toString = name
  def of(suite: Suite): Card = Card(suite,this)
}

object Rank {
  implicit def fromInt(rank: Int): Rank = rank match {
    case 2 => _2
    case 3 => _3
    case 4 => _4
    case 5 => _5
    case 6 => _6
    case 7 => _7
    case 8 => _8
    case 9 => _9
    case 10 => _10
    case _ => throw new IllegalArgumentException(s"rank must be between 2 and 10, $rank provided")
  }
}

object _2 extends Rank(2)
object _3 extends Rank(3)
object _4 extends Rank(4)
object _5 extends Rank(5)
object _6 extends Rank(6)
object _7 extends Rank(7)
object _8 extends Rank(8)
object _9 extends Rank(9)
object _10 extends Rank(10)
object Jack extends Rank(11,"J")
object Queen extends Rank(12,"Q")
object King extends Rank(13,"K")
object Ace extends Rank(14,"A")

case class Card(suite: Suite, rank: Rank) {
  override def toString = {
    import Card._

    val card: String = color(suite) + s"$rank$suite" + Console.RESET
    if(suite == Card.trumps)  Console.YELLOW + s"\u2654"  + card else card
  }
}

object Card {
  var trumps: Suite = null
  val black = Console.WHITE_B+ Console.BLACK
  val red = Console.WHITE_B + Console.RED
  val color = Map( Hearts -> red,  Diamonds -> red, Clubs -> black, Spades -> black)
}

object Deck {
  type Hand = mutable.Set[Card]
  type Batch = Set[Card]
  type Deck = List[Card]

  val HandSize = 13
  val DeckSize = 52

  val hearts: Batch = Set(
    Card(Hearts, 2),
    Card(Hearts, 3),
    Card(Hearts, 4),
    Card(Hearts, 5),
    Card(Hearts, 6),
    Card(Hearts, 7),
    Card(Hearts, 8),
    Card(Hearts, 9),
    Card(Hearts, 10),
    Card(Hearts, Jack),
    Card(Hearts, Queen),
    Card(Hearts, King),
    Card(Hearts, Ace)
  )

  val spades: Batch = Set(
    Card(Spades, 2),
    Card(Spades, 3),
    Card(Spades, 4),
    Card(Spades, 5),
    Card(Spades, 6),
    Card(Spades, 7),
    Card(Spades, 8),
    Card(Spades, 9),
    Card(Spades, 10),
    Card(Spades, Jack),
    Card(Spades, Queen),
    Card(Spades, King),
    Card(Spades, Ace)
  )

  val clubs: Batch = Set(
    Card(Clubs, 2),
    Card(Clubs, 3),
    Card(Clubs, 4),
    Card(Clubs, 5),
    Card(Clubs, 6),
    Card(Clubs, 7),
    Card(Clubs, 8),
    Card(Clubs, 9),
    Card(Clubs, 10),
    Card(Clubs, Jack),
    Card(Clubs, Queen),
    Card(Clubs, King),
    Card(Clubs, Ace)
  )

  val diamonds:Batch = Set(
    Card(Diamonds, 2),
    Card(Diamonds, 3),
    Card(Diamonds, 4),
    Card(Diamonds, 5),
    Card(Diamonds, 6),
    Card(Diamonds, 7),
    Card(Diamonds, 8),
    Card(Diamonds, 9),
    Card(Diamonds, 10),
    Card(Diamonds, Jack),
    Card(Diamonds, Queen),
    Card(Diamonds, King),
    Card(Diamonds, Ace)
  )

  val allCards: Batch = hearts ++ clubs ++ spades ++ diamonds
  def deck: Deck = Random.shuffle(Deck.allCards.toList)
  def deal: (Hand,Hand,Hand,Hand) = {
    val List(h1,h2,h3,h4) = deck.grouped(Deck.HandSize).toList
    (h1,h2,h3,h4)
  }

  implicit def toHand(cards:List[Card]): Hand = mutable.Set(cards: _*)
}