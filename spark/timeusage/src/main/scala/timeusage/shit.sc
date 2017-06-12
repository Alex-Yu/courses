trait Addable[T] {
  def plus(a: T, b: T): T
}

case class A(s: String, i: Int)

object Addable {

  implicit object AddableA extends Addable[A] {
    override def plus(a: A, b: A): A = A(a.s + b.s, a.i + b.i)
  }

  implicit class AddableExtend[T](a: T)(implicit ev: Addable[T]) {
    def +(that: T): T = ev.plus(a, that)
  }

}

val a = A("a", 1)
val b = A("b", 2)

import Addable._
def sum[R : Addable](a1: R, a2: R): R = a1 + a2

val c = a + b



