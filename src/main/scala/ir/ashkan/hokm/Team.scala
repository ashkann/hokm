package ir.ashkan.hokm

class Team(val player1: Player, val player2: Player) {
  var score: Int = 0
  def contains(player: Player): Boolean = player == player1 || player == player2
}