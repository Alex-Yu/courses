package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

 /* def printEls(h: H): Unit = {
    if (!isEmpty(h)) {
      print(findMin(h) + " ")
      printEls(deleteMin(h))
    } else println
  }

  property("print") = forAll { (h: H) =>
    printEls(h)
    true
  }*/

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("proper_order_for_two") = forAll { (e1: A, e2: A) =>
    findMin(insert(e2, insert(e1, empty))) == math.min(e1, e2)
  }

  property("delete_min") = forAll { e: A =>
    isEmpty(deleteMin(insert(e, empty)))
  }

  def isProperOrder(h: H) = {
    @tailrec
    def isProperOrderLoop(ht: H, e: A): Boolean =
      isEmpty(ht) || {
        val htMin = findMin(ht)
        e <= htMin && isProperOrderLoop(deleteMin(ht), htMin)
      }

    isEmpty(h) || isProperOrderLoop(deleteMin(h), findMin(h))
  }

  property("proper_order_for_all") = forAll { (h: H) =>
    isProperOrder(h)
  }

  property("melding1") = forAll { (h1: H, h2: H) =>
    (isEmpty(h1) && isEmpty(h2)) || {
      val meldMin = findMin(meld(h1, h2))
      if (isEmpty(h1)) meldMin == findMin(h2)
      else if (isEmpty(h2)) meldMin == findMin(h1)
      else meldMin == findMin(h1) || meldMin == findMin(h2)
    }
  }

  property("melding2") = forAll { (h1: H, h2: H) =>
    isProperOrder(meld(h1, h2))
  }

  property("flow") = forAll { h: H =>
    def toList(h: H): List[Int] = {
      def loop(ht: H, z: List[A]): List[A] = if (isEmpty(ht)) z else loop(deleteMin(ht), findMin(ht) :: z)
      loop(h, List.empty).reverse
    }

    def toHeap(l: List[A]): H = l.foldLeft(empty)((z, e) => insert(e, z))

    val l = toList(h)
    val h2 = toHeap(l)

    def isEqual(h1: H, h2: H): Boolean =
      (isEmpty(h1) && isEmpty(h2)) || (findMin(h1) == findMin(h2) && isEqual(deleteMin(h1), deleteMin(h2)))

    isEqual(h, h2)
  }





}
