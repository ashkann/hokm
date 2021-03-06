package ir.ashkan.hokm

class Suite private (val symbol:Char) {
  override def toString = s"$symbol"
  val cards: Set[Card] = Rank.ranks.map { _ of this }
  def cards(ordering:RankOrdering = RankOrdering.natural): Seq[Card] = Rank.ranks.toList.sorted(ordering).map { _ of this }
}

object Suite {
  val Hearts = new Suite('\u2665')
  val Spades = new Suite('\u2660')
  val Clubs = new Suite('\u2663')
  val Diamonds = new Suite('\u2666')

  val suites = Set(Hearts,Spades,Clubs,Diamonds)
  def suites(ordering: SuiteOrdering = SuiteOrdering.natural): Seq[Suite] = suites.toList.sorted(ordering)
}