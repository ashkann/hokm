package ir.ashkan.hokm

import scala.collection.mutable
import scala.util.Random

sealed abstract class Suite
object Hearts extends Suite
object Spades extends Suite
object Clubs extends Suite
object Diamonds extends Suite

sealed abstract class Rank(val rank: Int, val name:String) {
  def this(rank: Int) = this(rank,rank.toString)
  override def toString = name
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
//  override def toString = {
//    import Card._
//
//    val card: String = color(suite) + s"$rank$suite" + Console.RESET
//    if(suite == Card.trumps)  Console.YELLOW + s"\u2654"  + card else card
//  }
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
    Card(Hearts, _2),
    Card(Hearts, _3),
    Card(Hearts, _4),
    Card(Hearts, _5),
    Card(Hearts, _6),
    Card(Hearts, _7),
    Card(Hearts, _8),
    Card(Hearts, _9),
    Card(Hearts, _10),
    Card(Hearts, Jack),
    Card(Hearts, Queen),
    Card(Hearts, King),
    Card(Hearts, Ace)
  )

  val spades: Batch = Set(
    Card(Spades, _2),
    Card(Spades, _3),
    Card(Spades, _4),
    Card(Spades, _5),
    Card(Spades, _6),
    Card(Spades, _7),
    Card(Spades, _8),
    Card(Spades, _9),
    Card(Spades, _10),
    Card(Spades, Jack),
    Card(Spades, Queen),
    Card(Spades, King),
    Card(Spades, Ace)
  )

  val clubs: Batch = Set(
    Card(Clubs, _2),
    Card(Clubs, _3),
    Card(Clubs, _4),
    Card(Clubs, _5),
    Card(Clubs, _6),
    Card(Clubs, _7),
    Card(Clubs, _8),
    Card(Clubs, _9),
    Card(Clubs, _10),
    Card(Clubs, Jack),
    Card(Clubs, Queen),
    Card(Clubs, King),
    Card(Clubs, Ace)
  )

  val diamonds:Batch = Set(
    Card(Diamonds, _2),
    Card(Diamonds, _3),
    Card(Diamonds, _4),
    Card(Diamonds, _5),
    Card(Diamonds, _6),
    Card(Diamonds, _7),
    Card(Diamonds, _8),
    Card(Diamonds, _9),
    Card(Diamonds, _10),
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