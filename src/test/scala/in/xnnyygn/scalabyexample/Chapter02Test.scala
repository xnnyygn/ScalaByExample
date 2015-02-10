package in.xnnyygn.scalabyexample

import org.specs2.mutable._

class Chapter02Test extends Specification {
  "Chapter02" should {
    "sort [2, 3, 4, 5, 1] => [1, 2, 3, 4, 5]" in {
      Chapter02.quickSortFunctional(List(2, 3, 4, 5, 1)) must_== List(1, 2, 3, 4, 5)
    }
  }
}