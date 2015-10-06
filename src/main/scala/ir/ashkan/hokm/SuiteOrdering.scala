package ir.ashkan.hokm

abstract class SuiteOrdering extends Ordering[Suite] {
  def score(suite: Suite): Int
  def compare(left: Suite, right: Suite) = score(left) - score(right)
  def winner(hand:Set[Suite]): Suite = hand.max(this)
}

object SuiteOrdering {
  var orderingInEffect: SuiteOrdering = _
  def winner(hand: Set[Suite]) = orderingInEffect.winner(hand)
  def winner(hand: Suite*): Suite = winner(hand.toSet)

  def apply(trumps: Suite): SuiteOrdering = new TrumpSuiteOrder(trumps)
  def apply(s1: Suite, s2: Suite, s3: Suite, s4: Suite): SuiteOrdering = new ManualSuiteOrder(s1,s2,s3,s4)

  implicit class OrderedSuite(suite: Suite) extends Ordered[Suite] {
    def compare(that: Suite) = orderingInEffect.compare(suite,that)
    def takes(that: Suite) = this > that
    def takes(others: Set[Suite]): Boolean = others.forall { this takes _ }
  }

  object NeutralSuiteOrder extends SuiteOrdering {
    def score(suite: Suite) = 0
  }

  class TrumpSuiteOrder(trumps: Suite) extends SuiteOrdering {
    def score(suite: Suite) = if(suite == trumps) 1 else 0
  }

  class ManualSuiteOrder(s1: Suite, s2: Suite, s3: Suite, s4: Suite) extends SuiteOrdering {
    val scores = Map(s1->4,s2->3,s3->2,s4->1)
    def score(suite: Suite) = scores(suite)
  }
}