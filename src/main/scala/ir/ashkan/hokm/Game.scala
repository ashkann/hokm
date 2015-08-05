package ir.ashkan.hokm

import scala.Console.println
import scala.collection.SortedMap
import scala.util.Random

object Game extends App {
  import DSL._
  import ir.ashkan.hokm.Deck.{Batch, Deck, Hand}

  implicit class ConsoleContext(private val sc:StringContext) extends {
    def hokm(args: Any*): String = {
      val vs = args map {
        case team:Team => "Team"
        case _ => "Dunno!"
      }


      (vs mkString ",") + (sc.parts mkString "|")
    }
  }


  val (h1,h2,h3,h4) = Deck.deal
  val List(p1,p2,p3,p4) = Random.shuffle(List(
    new Player("Jake", h1),
    new Player("Bella", h2),
    new Player("Edward", h3),
    new Player("Ashkan", h4)
  ))
  val team1 = new Team(p1,p3)
  val team2 = new Team(p2,p4)
  val trumpCaller = team1.player1
  val trumpCallerTeamMate = team1.player2

  val Crown = '\u2654'
  val TrumpCallerTeamMateCrown = Crown + Console.RESET
  val TrumpCallerCrown = Console.BOLD + Console.YELLOW + TrumpCallerTeamMateCrown
  implicit val ordering = CardOrderingForConsole

  println(hokm"$team1 vs $team2")

  println(s"$team1 vs $team2")
  println(s"$trumpCaller, call trumps:")


  val trumps = pickCard(trumpCaller, 5).suite
  Card.trumps = trumps

  var lead = trumpCaller
  repeatUntil[Team] {
    val trick = new Trick(lead)
    playTrick(trick)
    lead = trick.winner

    val winnerTeam = team(lead)
    winnerTeam.score += 1
    println(s"$winnerTeam took the deal")

    winnerTeam
  } (winner => winner.score >= 7)


  class Player(val name: String, val hand: Hand) {
    // @todo Hand is mutable, but partition is immutable
    def partition(leadSuite: Suite): (Batch,Batch) = hand.toSet.partition(_.suite == leadSuite)

    def validCardsToPlay(leadSuite: Suite) = {
      val (followSuites, sluffs) = partition(leadSuite)
      if (followSuites.isEmpty) sluffs else followSuites
    }

    override def toString = this match {
      case `trumpCaller` => TrumpCallerCrown + name
      case `trumpCallerTeamMate` => TrumpCallerTeamMateCrown + name
      case _ => name
    }

    def playsIn(team: Team) = team contains this
  }

  class Team(val player1: Player, val player2: Player) {
    var score: Int = 0
    override def toString = s"$player1 and $player2"
    def contains(player: Player): Boolean = player == player1 || player == player2
  }

  class Trick(val lead: Player) {
    var leadCard, card2, card3, card4 : Card = _

    val second: Player = nextPlayer(lead)
    val third:  Player = nextPlayer(second)
    val fourth: Player = nextPlayer(third)
    lazy val leadSuite: Suite = leadCard.suite
    def winner: Player = playerOf(topCard)
    def topCard: Card = cards.max

    def update(index: Int, card: Card): Unit = index match {
      case 1 => leadCard = card
      case 2 => card2 = card
      case 3 => card3 = card
      case 4 => card4 = card
    }

    def apply(index: Int): Card = index match {
      case 1 => leadCard
      case 2 => card2
      case 3 => card3
      case 4 => card4
    }

    def player(index: Int): Player = index match {
      case 1 => lead
      case 2 => second
      case 3 => third
      case 4 => fourth
    }

    private def cards: Deck = List(leadCard, card2, card3, card4) filter { _ != null }

    private def playerOf(card: Card): Player = Map(
      leadCard->lead,
      card2->second,
      card3->third,
      card4->fourth
    )(card)

    private def nextPlayer(current: Player) = current match {
      case team1.player1 => team2.player1
      case team2.player1 => team1.player2
      case team1.player2 => team2.player2
      case team2.player2 => team1.player1
    }

    override def toString = {
      val plays = Map(lead->leadCard,second->card2,third->card3,fourth->card4) filter { case (_,card) => card != null }
      plays map { case (player,card)=> if(player == winner) Console.BOLD+s"$player $card"+Console.RESET else s"$player $card" } mkString "  "
    }
  }

  def team(player: Player): Team = if(player playsIn team1) team1 else team2

  def playTrick(trick: Trick) {
    println(s"${trick.lead}, you are the trick-leader. Play a card")
    trick(1) = pickCard(trick.lead)
    println(trick)

    for(turn <- 2 to 4) {
      println(s"${trick.player(turn)}, play a card")
      trick(turn) = pickCard(trick.player(turn),trick.leadSuite)
      println(trick)
    }
  }

  def pickCard(player: Player, howMany: Int = Deck.HandSize): Card = {
    require(howMany >0 && howMany <= Deck.HandSize)
    pickCard(player.hand.toList.take(howMany))
  }

  def pickCard(player: Player, leadSuite: Suite): Card = {
    val (valids,invalids) = player.partition(leadSuite)
    pickCard(valids,invalids)
  }

  def pickCard(valids: Batch, invalids: Batch): Card = {
    val menu = SortedMap(('a' to 'z').zip(valids.toList.sorted) :_*)
    val validChoices = menu.keys.toSet

    val choice = repeatUntil {
      val canPicks = menu map { case (choice, card) => s"$choice $card" } mkString " "
      val cantPicks = invalids.toList.sorted.mkString(" ")
      println(canPicks + " " + cantPicks)
      println("Which one ? ")
      scala.io.StdIn.readChar()
    } (choice => validChoices contains choice)

    menu(choice)
  }

  def pickCard(cards: Deck): Card = {
    val menu = SortedMap(('a' to 'z').zip(cards.sorted) :_*)
    val validChoices = menu.keys.toSet

    val choice = repeatUntil {
      println(menu map { case (choice, card) => s"$choice $card" } mkString " ")
      println("Which one ? ")
      val input = scala.io.StdIn.readChar()
      if(input == 'z') sys.exit(1)
      input
    } (choice => validChoices contains choice)

    menu(choice)
  }
}

object DSL {
  /**
   * Repeatedly performs a piece of code and checks the result until a condition holds
   * @param code The piece of code to execute
   * @param condition The condition to observe
   * @tparam T Type of the result that code returns
   * @return The result for the last execution of code for which the condition was True
   */
  def repeatUntil[T](code: =>T)(condition: T => Boolean): T = {
    code match {
      case result if condition(result) => result
      case _ => repeatUntil(code)(condition)
    }
  }
}

object Semantics {

}
