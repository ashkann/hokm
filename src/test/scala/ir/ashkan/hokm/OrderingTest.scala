package ir.ashkan.hokm

import org.scalatest.{Assertions, FunSuite}
import ir.ashkan.hokm.Suite.{Hearts,Spades,Diamonds,Clubs}
import ir.ashkan.hokm.Rank.{_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace}
import SuiteOrdering.OrderedSuite
import RankOrdering.OrderedRank
import CardOrdering.OrderedCard
import Rank.fromInt

class OrderingTest extends FunSuite with Assertions {

  test("Trumps take plain suites") {
    SuiteOrdering.orderingInEffect = SuiteOrdering(Spades)

    assert(Spades takes Diamonds)
    assert(Spades takes Clubs)
    assert(Spades takes Hearts)

    assert(Spades == Set(Diamonds,Spades,Hearts,Clubs).max)
  }

  test("Higher ranks take lower ranks") {
    val ascending : Seq[Rank] = Seq(2,3,4,5,6,7,8,9,10,Jack,Queen,King,Ace)
    val descending : Seq[Rank] = Seq(Ace,King,Queen,Jack,10,9,8,7,6,5,4,3,2)

    assert( descending == ascending.sortWith { _ > _ } )
  }

  test("A trump card takes *all* plain cards (even plain cards with higher ranks)") {
    CardOrdering.orderingInEffect = CardOrdering(SuiteOrdering(Spades),RankOrdering.naturalOrder)

    val winner = 2 of Spades
    val looser1 = 3 of Hearts
    val looser2 = Queen of Diamonds
    val looser3 = King of Clubs

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