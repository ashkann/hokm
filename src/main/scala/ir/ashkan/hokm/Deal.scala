package ir.ashkan.hokm

import scala.collection.mutable.ArrayBuffer

class Deal {
  val tricks = List()
  private val hands = ArrayBuffer[Team]()

  def score(t: Team): Int = hands.count(_ == t)
  def trickFinished(winner: Team) { hands += winner }
  def lastWinner = hands.last
  def lastWinnerScore = score(lastWinner)
}