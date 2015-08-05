package ir.ashkan.hokm

abstract class SuiteOrdering extends Ordering[Suite] {
  def score(suite: Suite): Int
  def compare(left: Suite, right: Suite) = score(left) - score(right)
}

object SuiteOrdering {
  var orderingInEffect: SuiteOrdering = _

  def apply(trumps: Suite): SuiteOrdering = new TrumpSuiteOrder(trumps)

  implicit class OrderedSuite(suite: Suite) extends Ordered[Suite] {
    def compare(that: Suite) = orderingInEffect.compare(suite,that)
  }

  object NeutralSuiteOrder extends SuiteOrdering {
    def score(suite: Suite) = 0
  }

  class TrumpSuiteOrder(trumps: Suite) extends SuiteOrdering {
    def score(suite: Suite) = if(suite == trumps) 1 else 0
  }
}