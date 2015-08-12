package ir.ashkan.hokm
import Renderer._

object Check extends App {
  val symbols = s"${Hearts} ${Spades} ${Diamonds} ${Clubs}"
  val red: String = Console.WHITE_B + Console.RED + " RED " + Console.RESET
  val white: String = Console.RED_B + Console.WHITE + " WHITE " + Console.RESET

  println("This checks for proper rendering support for your console")

  println(red + white)
  println {
    sys.props.get("file.encoding") match {
      case Some("UTF-8") => symbols
      case Some("UTF8") => symbols
      case Some(bad) => s"`file.encoding` must read 'UTF-8' or 'UTF8', but it actually reads '$bad'"
      case None => "`file.encoding` must be 'UTF-8' or 'UTF8', but it is empty"
    }
  }

  for( suite <- Seq(Hearts,Clubs,Diamonds,Spades)) {
    val cs = Seq(_2,_3,_4,_5,_6,_7,_8,_9,_10,Jack,Queen,King,Ace) map { rank => h"${Card(suite,rank)}" }
    println(cs mkString " ")
    println()
  }
}
