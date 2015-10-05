package ir.ashkan.hokm

sealed abstract class Suite private (val symbol:Char) {
  override def toString = s"$symbol"
  val cards: Set[Card] = Rank.ranks.map { _ of this }
}

object Suite {
  object Hearts extends Suite('\u2665')
  object Spades extends Suite('\u2660')
  object Clubs extends Suite('\u2663')
  object Diamonds extends Suite('\u2666')

  val suites = Set(Hearts,Spades,Clubs,Diamonds)
}
