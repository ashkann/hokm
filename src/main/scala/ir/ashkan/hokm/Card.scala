package ir.ashkan.hokm

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