package ir.ashkan.hokm

import org.scalatest.{Assertions, FunSuite}

class OrderingTest extends FunSuite with Assertions {
  import SuiteOrdering.OrderedSuite
  import RankOrdering.OrderedRank
  import CardOrdering.OrderedCard

  test("Trumps are above all") {
    SuiteOrdering.orderingInEffect = SuiteOrdering(Spades)

    assert(Spades > Diamonds)
    assert(Spades > Clubs)
    assert(Spades > Hearts)

    assert(Spades == Set(Diamonds,Spades,Hearts,Clubs).max)
  }

  test("Ranks are naturally ordered") {
    val smallerToBigger = Seq(_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace)

    assert( Seq(Ace,King,Queen,Jack,_10,_9,_8,_7,_6,_5,_4,_3,_2) == smallerToBigger.sortWith { _ > _ } )
  }

  test("A trump card wins over all cards from other suites (including higher ranks)") {
    CardOrdering.orderingInEffect = CardOrdering(SuiteOrdering(Spades),RankOrdering.naturalOrder)

    val winner  = Card(Spades, _2)
    val looser1 = Card(Hearts, Ace)
    val looser2 = Card(Diamonds, Ace)
    val looser3 = Card(Clubs, Ace)

    assert( winner > looser1)
    assert( winner > looser2)
    assert( winner > looser3)

    assert( winner == Set(looser3, looser1, winner, looser2).max )
  }

  test("Arbitrarily ordering suites") {
    val ord = SuiteOrdering(Hearts,Spades,Clubs,Diamonds)
    val ascending = Seq(Diamonds,Clubs,Spades,Hearts)
    val descending = ascending.reverse

    assert( ascending == descending.sorted(ord) )
  }
}