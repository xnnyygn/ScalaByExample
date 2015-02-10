package in.xnnyygn.scalabyexample

object Chapter02 {

  def quickSortFunctional[A](xs: List[A])(implicit ord: Ordering[A]): List[A] = {
    xs match {
      case Nil => xs
      case pivot :: tail => {
        val (left, right) = tail.partition(ord.lt(_, pivot))
        quickSortFunctional(left) ::: pivot :: quickSortFunctional(right)
      }
    }
  }

}