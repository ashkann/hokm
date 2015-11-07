package ir.ashkan.hokm

import ir.ashkan.hokm.Rank.{_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace}

abstract class RankOrdering extends Ordering[Rank] {
  def score(rank: Rank): Int
  def compare(left: Rank, right: Rank) = score(left) - score(right)
  def winner(hand: Set[Rank]): Rank = hand.max(this)
}

object RankOrdering {
  def winner(hand: Set[Rank]) = orderingInEffect.winner(hand)
  def winner(hand: Rank*) = orderingInEffect.winner(hand.toSet)

  val natural = new RankOrdering {
    private val scores = Seq(_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace).zipWithIndex.toMap

    override def score(rank: Rank): Int = scores(rank)
  }

  var orderingInEffect: RankOrdering = natural

  implicit class OrderedRank(rank: Rank) extends Ordered[Rank] {
    def compare(that: Rank) = orderingInEffect.compare(rank,that)
    def takes(that: Rank) = this > that
    def takes(others: Set[Rank]): Boolean = others.forall { this takes _ }
  }
}