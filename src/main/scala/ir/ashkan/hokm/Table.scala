package ir.ashkan.hokm

import scala.collection.mutable
import scala.util.Random

/**
  * Created by ashkan on 9/15/16.
  */
object Table {
  def deal: (Team,Team) = {
    import ir.ashkan.hokm.Deck.{Hand => H}

    def toHand(cards:List[Card]): H = mutable.Set(cards: _*)

    val List(h1,h2,h3,h4) = Deck.deck.grouped(Deck.HandSize).toList

    val List(p1,p2,p3,p4) = Random.shuffle(List(
      new Player("Jake", toHand(h1)),
      new Player("Bella", toHand(h2)),
      new Player("Edward", toHand(h3)),
      new Player("Ashkan", toHand(h4))
    ))

    (new Team(p1,p3),new Team(p2,p4))
  }
}
