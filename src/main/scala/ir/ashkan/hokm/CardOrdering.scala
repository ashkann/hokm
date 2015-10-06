package ir.ashkan.hokm

class CardOrdering(suites: SuiteOrdering, ranks: RankOrdering) extends Ordering[Card] {
  def compare(left: Card, right: Card) = suites.compare(left.suite, right.suite) match {
    case 0 => ranks.compare(left.rank, right.rank)
    case winnerSuite => winnerSuite
  }
}

object CardOrdering {
  var orderingInEffect: CardOrdering = new CardOrdering(SuiteOrdering.orderingInEffect,RankOrdering.orderingInEffect) {}

  def apply(suites: SuiteOrdering, ranks: RankOrdering) = new CardOrdering(suites,ranks)

  implicit class OrderedCard(card: Card) extends Ordered[Card] {
    def compare(that: Card) = orderingInEffect.compare(card,that)
    def takes(that: Card) = this > that
    def takes(others: Set[Card]): Boolean = others.forall { this takes _ }
  }
}