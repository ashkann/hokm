package ir.ashkan.hokm
class Renderer
(
  val background: String,
  val black: String,
  val red: String
  ) extends (Suite => String) with ((Suite, Rank) => String) {

  def apply(s: Suite): String = render(s, s.toString)

  def apply(s: Suite, r: Rank): String = render(s, r.toString + s.toString)

  private def render(s: Suite, payload: String): String = background + (s match {
    case _ if s == Clubs || s == Spades => black
    case _ if s == Diamonds || s == Hearts => red
  }) + payload + Console.RESET
}

object Renderer {

  //      (r match {
  //    case _2 => "2"
  //    case _3 => "3"
  //    case _4 => "4"
  //    case _5 => "5"
  //    case _6 => "6"
  //    case _7 => "7"
  //    case _8 => "8"
  //    case _9 => "9"
  //    case _10 => "10"
  //    case Jack => "J"
  //    case Queen => "Q"
  //    case King => "K"
  //    case Ace => "A"
  //    }
  //      )

  implicit class HokmStringContext(private val sc: StringContext) {
    def h(args: Any*): String = {
      val interleaved = (sc.parts zip args).flatMap { case (arg, part) => Seq(arg, part) } ++ sc.parts.takeRight(sc.parts.length - args.length)

      (interleaved map {
        case s: Suite => renderer(s)
        case Card(s, r) => renderer(s, r)
        case other => other.toString
      }).mkString
    }
  }

  val renderer = new Renderer(Console.WHITE_B, Console.BLACK, Console.RED)
}