package scala.ir.ashkan.hokm

/**
 * Created by ashkan on 8/3/15.
 */
object Learn extends App {
  implicit def typeEqCommutes[A,B](ev: A=:=B): B=:=A = ev.asInstanceOf[B=:=A]

  class Pair[T, S](var first: T, var second: S) {
    def swap(implicit ev: S =:= T) {
      implicit val ev2 = typeEqCommutes(ev)

      val temp = first
      first = second //error: type mismatch; found: S required: T
      second = temp
    }
  }
}
