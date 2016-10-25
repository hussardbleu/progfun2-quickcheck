package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield meld(insert(a, empty), h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  /** If you insert
    * an element into an empty heap, then find the minimum
    * of the resulting heap, you get the element back
    */

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /** If you insert any two elements into an empty heap,
  * finding the minimum of the resulting heap should get
  * the smallest of the two elements back.
  */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a | b)
  }

  /**If you insert an element into an empty heap,
  * then delete the minimum, the resulting heap
  * should be empty.
  */

  /** Given any heap, you should get a sorted sequence of elements
  * when continually finding and deleting minima.
  * (Hint: recursion and helper functions are your friends.)
  */

  /**  Finding a minimum of the melding of any two heaps should
  * return a minimum of one or the other.
  */




}
