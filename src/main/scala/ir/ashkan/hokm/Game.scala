package ir.ashkan.hokm

import scala.Console.println
import DSL._
import ir.ashkan.layout.{Element, Thick}

object Game extends App {
  val name = scala.io.StdIn.readLine("What's your name? ")
  val p1 = Player(name)
  val p2 = Player("Bella")
  val p3 = Player("Edward")
  val p4 = Player("Ashkan")

  val table = new Table(Team(p1,p2),Team(p3,p4))

  val ui = new Terminal2D {
    val cardOrdering = CardOrdering.natural
    val goldPlayer = table.team1.player1
    val silverPlayer = table.team1.player2
  }

  val trumpCaller = table.team1.player1

  val trumps = callTrumps
  ui.trumps = trumps

    val round = playDeal(trumpCaller)
  //  println(ui(round.lastWinner) + " took the round")

  def callTrumps: Suite = {
    println(Thick(Element("call trumps")) )

    ui.pick(table.hand(trumpCaller).take(5).toSeq.sorted(CardOrdering.natural),Nil).suite
  }

  def playDeal(eldest: Player): Deal = {
    val deal = new Deal

    repeatUntil[Deal,Player] { lead =>
      val trick = playTrick(lead)

      deal.trickFinished(trick.takerTeam)
      println(ui(trick.takerTeam) + " took the trick")

      println( ui(table.team1) beside Element(deal.score(table.team1).toString) )
      println( ui(table.team2) beside Element(deal.score(table.team2).toString) )

      (deal,trick.taker)
    }(_.lastWinnerScore >= 1)(eldest)
  }

  def playTrick(lead: Player): Trick = {
    val trick = new Trick(lead, table.team1, table.team2, CardOrdering(trumps))

    println(ui(lead) + ", lead")
    trick(lead) = ui.pick(table.hand(lead).toSeq.sorted(CardOrdering(trumps)),Nil)
    println(ui(trick))

    for (player <- trick.others) {
      println(ui(player) + ", play a card")
      trick(player) = ui.pick(table.hand(player).toSeq.sorted(CardOrdering(trumps)),Nil)
      println(ui(trick))
    }
    trick
  }


}