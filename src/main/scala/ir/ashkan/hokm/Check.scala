package scala.ir.ashkan.hokm

/**
 * Created by ashkan on 8/1/15.
 */
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
}
