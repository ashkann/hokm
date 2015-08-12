package ir.ashkan.hokm

import org.scalatest._

class GameTest extends FunSuite with Matchers {

  class Renderer
  (
    val background: String,
    val black: String,
    val red: String
    ) extends (Suite => String) with ((Suite, Rank) => String) {
    def apply(s: Suite): String = render(s, s.toString)

    def apply(s: Suite, r: Rank): String = render(s, s.toString + r.toString)

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
    private def render(s: Suite, payload: String): String = (s match {
      case _ if s == Clubs || s == Spades => black
      case _ if s == Diamonds || s == Hearts => red
    }) + payload + Console.RESET
  }

  implicit class HokmStringContext(private val sc: StringContext) {

    import HokmStringContext._

    def h(args: Any*): String = {
      val interleaved = (sc.parts zip args).flatMap { case (arg, part) => Seq(arg, part) } ++ sc.parts.takeRight(sc.parts.length - args.length)

      (interleaved map {
        case s: Suite => renderer(s)
        case Card(s, r) => renderer(s, r)
        case other => other.toString
      }).mkString
    }
  }

  object HokmStringContext {
    val renderer = new Renderer(Console.WHITE_B, Console.BLACK, Console.RED)
  }

  test("Empty strings") {
    h"" should be("")
  }

  test("Pure string (no interpolation involved)") {
    h"no interpolation" should be(s"no interpolation")
  }

  test("just a single injection") {
    h"${1}" should be(s"${1}")
  }

  test("Two adjacent injections") {
    h"${1}${2}" should be(s"${1}${2}")
  }

  test("Interleaved injections") {
    h"first number is ${1}, and then there is ${2}!" should be(s"first number is ${1}, and then there is ${2}!")
  }

  test("Suites are rendered by HokmStringContext.suiteRenderer") {
    val renderer = HokmStringContext.renderer

    h"$Hearts" should be(renderer(Hearts))
    h"$Diamonds" should be(renderer(Diamonds))
    h"$Clubs" should be(renderer(Clubs))
    h"$Spades" should be(renderer(Spades))
  }

  test("Cards are rendered by HokmStringContext.suiteRenderer") {
    val renderer = HokmStringContext.renderer

    h"${Card(Hearts, Ace)}" should be(renderer(Hearts, Ace))
  }
}