package ir.ashkan.hokm

import org.scalatest.{Assertions, FunSuite}

class OrderingTest extends FunSuite with Assertions {
  import SuiteOrdering.OrderedSuite
  import RankOrdering.OrderedRank

  test("Trumps are above all") {
    SuiteOrdering.orderingInEffect = SuiteOrdering(Spades)

    assert(Spades > Diamonds)
    assert(Spades > Clubs)
    assert(Spades > Hearts)

    assert(Spades == Set(Diamonds,Hearts,Clubs,Spades).max)
  }

  test("Ranks are naturally ordered") {
    val smallerToBigger = Seq(_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace)

    assert( Seq(Ace,King,Queen,Jack,_10,_9,_8,_7,_6,_5,_4,_3,_2) == smallerToBigger.sortWith { _ > _ } )
  }
}
