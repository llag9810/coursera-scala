package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      value <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(value, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (h: H, x: Int, y: Int) =>
    findMin(insert(y, insert(x, h))) == math.min(x, y)
  }

  property("empty1") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("min3") = forAll { (a: H, b: H) =>
    findMin(meld(a, b)) == math.min(findMin(a), findMin(b))
  }
}
