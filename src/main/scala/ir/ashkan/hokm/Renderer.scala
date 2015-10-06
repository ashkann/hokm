package ir.ashkan.hokm
import ir.ashkan.hokm.Suite.{Hearts,Spades,Diamonds,Clubs}

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
  implicit class HokmStringContext(private val sc: StringContext) {
    def h(args: Any*): String = {
      val as=args.iterator
      val ps=sc.parts.iterator
      val b = StringBuilder.newBuilder
      while(as.hasNext) {
        b ++= ps.next()
        b ++= (as.next() match {
          case s: Suite => renderer(s)
          case Card(s, r) => renderer(s, r)
          case other => other.toString
        })
      }
      b ++= ps.next()

      b.toString()
    }
  }
  val renderer1 = new Renderer(Console.WHITE_B, Console.BLACK, Console.RED)
  val renderer2 = new Renderer(Console.YELLOW_B, Console.CYAN, Console.BLUE)
  var renderer = renderer2
}