package ir.ashkan.hokm

/**
  * Created by ashkan on 5/2/16.
  */
object TeamMembership extends App {
  class Player
  class Team(p1:Player, p2:Player) {
    case class Membership private[Team](val p:Player)

    val p1IsAMember = Membership(p1)
    val p2IsAMember = Membership(p2)

    def position(ev: Membership) = ev.p match {
      case `p1`=>1
      case `p2`=>2
    }
  }

  val p1 = new Player
  val p2 = new Player
  val p3 = new Player
  val t = new Team(p1,p2)

  println { t.position(t.p1IsAMember) }

  try {
    println { t.position(t.p2IsAMember) }
  } catch {
    case e:Throwable => println(e.getMessage)
  }
}
