package in.xnnyygn.scalabyexample

object Chapter06 {

  trait IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean

    def union(that: IntSet): IntSet
    def intersection(that: IntSet): IntSet

    def excl(x: Int): IntSet
    def isEmpty: Boolean
  }

  case object EmptySet extends IntSet {
    def incl(x: Int): IntSet = new NotEmptySet(x)
    def contains(x: Int): Boolean = false

    def union(that: IntSet): IntSet = that
    def intersection(that: IntSet): IntSet = EmptySet

    def excl(x: Int): IntSet = this
    def isEmpty: Boolean = true
  }

  case class NotEmptySet(elem: Int, left: IntSet = EmptySet, right: IntSet = EmptySet) extends IntSet {
    def incl(x: Int): IntSet = {
      if(x < elem) new NotEmptySet(elem, left.incl(x), right)
      else if(x > elem) new NotEmptySet(elem, left, right.incl(x))
      else this
    }
    def contains(x: Int): Boolean = {
      if(x < elem) left.contains(x)
      else if(x > elem) right.contains(x)
      else true
    }

    def union(that: IntSet): IntSet = right.union(left.union(that.incl(elem)))
    def intersection(that: IntSet): IntSet = {
      val lr = left.intersection(that) union right.intersection(that)
      if(that.contains(elem)) lr.incl(elem)
      else lr
    } 

    def excl(x: Int): IntSet = {
      if(x < elem) new NotEmptySet(elem, left.excl(x), right)
      else if(x > elem) new NotEmptySet(elem, left, right.excl(x))
      else left union right
    }
    def isEmpty: Boolean = false
  }

  object IntSet {
    def apply(xs: Int*): IntSet = {
      var s: IntSet = EmptySet
      for(x <- xs) s = s.incl(x)
      s
    }
  }
}