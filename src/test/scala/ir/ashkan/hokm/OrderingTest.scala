package ir.ashkan.hokm

import org.scalatest.{Assertions, FunSuite}

import scala.ir.ashkan.hokm._

/**
 * Created by ashkan on 8/5/15.
 */
class OrderingTest extends FunSuite with Assertions {
  test("when a trump is set, trump suite is above all") {
    val smaller = Card(Hearts,Ace)
    val bigger = Card(Diamonds,_2)

    assert (1 < 2)
  }

  test("otherwise, cards of the same rank are equal") {

  }
}
