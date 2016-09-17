package ir.ashkan.hokm

case class Team(player1: Player, player2: Player) {
   def contains(player: Player): Boolean = player == player1 || player == player2
}

