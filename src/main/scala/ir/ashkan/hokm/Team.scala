package ir.ashkan.hokm

class Team(val player1: Player, val player2: Player) {
   def contains(player: Player): Boolean = player == player1 || player == player2
}

