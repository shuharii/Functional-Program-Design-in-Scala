package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] = for {
    v <- Arbitrary.arbitrary[A]   // random elements A
    m <- Gen.oneOf(Gen.const(empty),genHeap) // empty, or a heap from a recursive call to this function
  } yield insert(v,m)
  given Arbitrary[H] = Arbitrary(genHeap)

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty
  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.

  property("prop") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) then true
      else findMin(h1) == findMin(h2) && heapEqual(deleteMin(h1), deleteMin(h2))
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
