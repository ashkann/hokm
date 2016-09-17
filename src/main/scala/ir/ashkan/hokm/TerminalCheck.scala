package ir.ashkan.hokm

import ir.ashkan.hokm.Suite._
import ir.ashkan.hokm.Rank._
import ir.ashkan.layout.Element
import scala.collection.mutable.{Set => Hand}

/**
  * Created by ashkan on 5/8/16.
  */
object TerminalCheck extends App { app =>
  val goldPlayer = Player("Ashkan")
  val silverPlayer = Player("Sepideh")
  val player3 = Player("Ramin")
  val player4 = Player("Shabnam")

  val ui = new Terminal2D {
    val cardOrdering = CardOrdering.natural
    val goldPlayer = app.goldPlayer
    val silverPlayer = app.silverPlayer
  }

  println (Suite.suites().foldRight(Element.Nil){ (suite,deck) => ui(suite.cards()) above deck })

  val table = new Table(Team(goldPlayer,silverPlayer),Team(player3,player4))
  println ( ui(table.team1) beside Element(" vs ") beside ui(table.team2))
  ui.pick(table.ph(goldPlayer).toSeq.sorted(CardOrdering.natural),Seq[Card]())
}