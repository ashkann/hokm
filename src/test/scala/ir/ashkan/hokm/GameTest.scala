//package ir.ashkan.hokm
//
//import ir.ashkan.hokm.Game.Round
//import org.scalatest._
//
//import scala.collection.mutable
//
///**
//  * Created by ashkan on 2/15/16.
//  */
//class GameTest extends FreeSpec with Matchers  {
//
//  val player1: Player = new Player("p1",mutable.Set[Card]())
//  val player2: Player = new Player("p2",mutable.Set[Card]())
//  val player3: Player = new Player("p3",mutable.Set[Card]())
//  val player4: Player = new Player("p4",mutable.Set[Card]())
//
//  val team1 = new Team(player1,player2)
//  val team2 = new Team(player3,player4)
//
//  "A round" - {
//
//    "have no tricks when just created" in {
//      val round = new Round(team1,team2)
//      assert(round.tricks.isEmpty)
//    }
//
//    "remembers the tricks played so far" in {
//      val round = new Round(team1,team2)
//      round.tricks += new Trick()
//    }
//
//    "have two participating teams" in {
//      val round= new Round(team1, team2)
//      assert(round.team1 == team1)
//      assert(round.team2 == team2)
//    }
//
//  }
//
//
//}