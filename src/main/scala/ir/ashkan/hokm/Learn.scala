package ir.ashkan.hokm


object Learn extends App {

  class Outter {
    type Inner = Either[Int,String]
    type L = Left[Int,String]
    type R = Right[Int,String]

    def f(x: Inner) = 1
  }

  val o = new Outter
  o.f(new o.L(1))
  o.f(new o.R("name"))

//  case class A()
//  case class B(a: A) {
//    def f(x: a.type) = true
//  }
//
//  val a = A()
//  val b = B(a)
//  b.f(b.a)


//  val p1 = new Player("",Deck.emptyHand)
//  val p2 = new Player("",Deck.emptyHand)
//  val p3 = new Player("",Deck.emptyHand)
//
//  val team = new Team(p1,p2)
//
//  val ev = new team.P
//  team.position(team.player1)
//  team.position(team.player2)
//  team.position(p3)

//  team.position2(team.player1)(team.ev1)
//  team.position2(team.player2)
//  team.position2(p3)
}
