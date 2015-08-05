package ir.ashkan.hokm

abstract class RankOrdering extends Ordering[Rank] {
  def score(rank: Rank): Int
  def compare(left: Rank, right: Rank) = score(left) - score(right)
}

object RankOrdering {
  val naturalOrder = new RankOrdering {
    private val scores = Seq(_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace).zipWithIndex.toMap

    override def score(rank: Rank): Int = scores(rank)
  }

  var orderingInEffect: RankOrdering = naturalOrder

  implicit class OrderedRank(rank: Rank) extends Ordered[Rank] {
    def compare(that: Rank) = orderingInEffect.compare(rank,that)
  }
}