package ir.ashkan.hokm

import org.scalatest._
import Renderer._

class GameTest extends FunSuite with Matchers {

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

  test("Suites are rendered by Renderer.renderer") {
    h"$Hearts" should be(renderer(Hearts))
    h"$Diamonds" should be(renderer(Diamonds))
    h"$Clubs" should be(renderer(Clubs))
    h"$Spades" should be(renderer(Spades))
  }

  test("Cards are rendered by Renderer.renderer") {
    h"${Card(Hearts, Ace)}" should be(renderer(Hearts, Ace))
  }
}