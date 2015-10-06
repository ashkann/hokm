package ir.ashkan.hokm

import org.scalatest.{Assertions, FunSuite}
import ir.ashkan.hokm.Suite.{Hearts,Spades,Diamonds,Clubs}
import ir.ashkan.hokm.Rank.{_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace}
import SuiteOrdering.OrderedSuite
import RankOrdering.OrderedRank
import CardOrdering.OrderedCard
import Rank.fromInt

class OrderingTest extends FunSuite with Assertions {

  test("Trump takes plain suites") {
    SuiteOrdering.orderingInEffect = SuiteOrdering(Spades)

    assert(Spades takes Diamonds)
    assert(Spades takes Clubs)
    assert(Spades takes Hearts)

    assert(Spades takes Set(Diamonds,Hearts,Clubs))
  }

  test("Higher ranks take lower ranks") {
    assert( Ace takes Rank.ranks.filter { _  != Ace })
    assert( King takes Queen)
    assert( Queen takes Jack )
    assert( Jack takes Set(_10,_9,_8) )
    assert( _4  takes Set(_3,_2) )
  }

  test("Trumps take all plain cards") {
    CardOrdering.orderingInEffect = CardOrdering(SuiteOrdering(Spades),RankOrdering.naturalOrder)

    val trump = 2 of Spades
    assert( trump takes (3 of Hearts))
    assert( trump takes (Queen of Diamonds))
    assert( trump takes (King of Clubs))

    assert( (10 of Spades) takes Set(2 of Clubs, Ace of Hearts, King of Diamonds) )
  }

  test("Arbitrarily ordering suites") {
    val ord = SuiteOrdering(Hearts,Spades,Clubs,Diamonds)
    val ascending = Seq(Diamonds,Clubs,Spades,Hearts)
    val descending = ascending.reverse

    assert( ascending == descending.sorted(ord) )
  }
}