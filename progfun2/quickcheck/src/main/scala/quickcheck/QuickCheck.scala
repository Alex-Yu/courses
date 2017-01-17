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

  property("proper_order_for_all") = forAll { (h: H) =>
    @tailrec
    def isProperOrder(ht: H, e: A): Boolean =
      if (isEmpty(ht)) true
      else {
        val htMin = findMin(ht)
        if (e <= htMin) isProperOrder(deleteMin(ht), htMin) else false
      }

    if (isEmpty(h)) true else isProperOrder(deleteMin(h), findMin(h))
  }

  property("melding") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val meldMin = findMin(meld(h1, h2))
      if (isEmpty(h1)) meldMin == findMin(h2)
      else if (isEmpty(h2)) meldMin == findMin(h1)
      else meldMin == findMin(h1) || meldMin == findMin(h2)
    }
  }



}
