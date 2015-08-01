package scala.ir.ashkan.hokm

class CardOrdering(suiteOrdering: Ordering[Suite], rankOrdering: Ordering[Rank]) extends Ordering[Card] {
  override def compare(left: Card, right: Card) = if(left.suite == right.suite)
    rankOrdering.compare(left.rank,right.rank)
  else
    suiteOrdering.compare(left.suite,right.suite)
}

object CardOrderingForConsole extends CardOrdering(SuiteOrderingForConsole,RankOrderingForConsole)

object SuiteOrderingForConsole extends Ordering[Suite] {
  private val order: Map[Suite,Int] = List(Hearts,Spades,Diamonds,Clubs).zipWithIndex.toMap
  override def compare(left: Suite, right: Suite) = order(left) - order(right)
}

object RankOrderingForConsole  extends Ordering[Rank] {
  override def compare(left: Rank, right: Rank) = right.rank - left.rank
}