package ir.ashkan.hokm

import ir.ashkan.hokm.Suite._
import ir.ashkan.hokm.Rank._
import ir.ashkan.layout.Element
import scala.collection.mutable.{Set => Hand}

/**
  * Created by ashkan on 5/8/16.
  */
object TerminalCheck extends App { app =>
  val goldPlayer = new Player("Ashkan", Hand(Ace of Spades))
  val silverPlayer = new Player("Sepideh", Hand(Ace of Spades))
  val player3 = new Player("Ramin", Hand(Ace of Spades))
  val player4 = new Player("Shabnam", Hand(Ace of Spades))

  val ui = new Terminal2D {
    val cardOrdering = CardOrdering.natural
    val goldPlayer = app.goldPlayer
    val silverPlayer = app.silverPlayer
  }

  println (Suite.suites().foldRight(Element.Nil){ (suite,deck) => ui(suite.cards()) above deck })
  println ( ui(new Team(goldPlayer,silverPlayer)) besides Element(" vs ") besides ui(new Team(player3,player4)))
}