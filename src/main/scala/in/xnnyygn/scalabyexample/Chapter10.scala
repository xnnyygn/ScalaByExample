package in.xnnyygn.scalabyexample

object Chapter10 {

  def queens2(n: Int): List[List[Int]] = {
    val allColumns = List.range(1, n + 1).toSet
    def calculateAvailableColumns(queens: List[Int]): Set[Int] = {
      allColumns -- queens.zipWithIndex.flatMap{
        case (x, i) => List(x - i - 1, x, x + i + 1)
      }
    }
    def placeQueens(k: Int): List[List[Int]] = {
      if (k == 1) allColumns.map(List(_)).toList
      else for{ queens <- placeQueens(k - 1)
                column <- calculateAvailableColumns(queens)} 
                yield column :: queens
    }
    placeQueens(n)
  }

  def queens(n: Int): List[List[Int]] = {
    val ys = List.range(1, n + 1)
    def availableY(qy: List[Int]): List[Int] = {
      val sy = qy.zipWithIndex.flatMap{
        case (y, i) => List(y - (i + 1), y, y + (i + 1))
      }.toSet
      ys.filterNot(sy.contains)
    }
    def placeQueens(m: Int): List[List[Int]] = {
      if(m == 1) ys.map(List(_))
      else placeQueens(m - 1).flatMap(s => availableY(s).map(_ :: s))
    }
    placeQueens(n)
  }

}