package in.xnnyygn.scalabyexample

import org.specs2.mutable._

class Chapter06Test extends Specification {
  "Chapter06" should {
    import Chapter06._

    "IntSet union" in {
      val a = IntSet(1, 3, 4, 5)
      val b = IntSet(2, 3, 1)
      val c = a union b
      c.contains(1) must beTrue
      c.contains(2) must beTrue
      c.contains(3) must beTrue
      c.contains(4) must beTrue
      c.contains(5) must beTrue
    }

    "IntSet intersection" in {
      val a = IntSet(1, 3, 4, 5)
      val b = IntSet(2, 3, 1)
      val c = a intersection b
      c.contains(1) must beTrue
      c.contains(3) must beTrue
    }

    "IntSet excl" in {
      val a = IntSet(1, 3, 4, 5)
      a.contains(3) must beTrue
      val b = a.excl(3)
      b.contains(3) must beFalse
    }
  }
}