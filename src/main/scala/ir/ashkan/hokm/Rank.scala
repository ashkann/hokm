package ir.ashkan.hokm

sealed abstract class Rank(val rank: Int, val name:String) {
  def this(rank: Int) = this(rank,rank.toString)
  override def toString = name
  def of(suite: Suite): Card = Card(suite,this)
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
  val ranks = Set(_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace)
}