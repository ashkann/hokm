package ir.ashkan.hokm

class Rank private (val rank: Int, val name: String) {
  override def toString = name
  def of(suite: Suite): Card = Card(suite,this)
}

object Rank {
  val _2 = new Rank(2,"2")
  val _3 = new Rank(3,"3")
  val _4 = new Rank(4,"4")
  val _5 = new Rank(5,"5")
  val _6 = new Rank(6,"6")
  val _7 = new Rank(7,"7")
  val _8 = new Rank(8,"8")
  val _9 = new Rank(9,"9")
  val _10 = new Rank(10,"10")
  val Jack = new Rank(11,"J")
  val Queen = new Rank(12,"Q")
  val King = new Rank(13,"K")
  val Ace = new Rank(14,"A")
  
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