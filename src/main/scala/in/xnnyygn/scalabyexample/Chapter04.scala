package in.xnnyygn.scalabyexample

import scala.math

object Chapter04 {

  // y, x / y, (y + x / y) / 2, y = 1
  def sqrt(x: Double): Double = {
    def newTonSqrt(x: Double, y: Double, n: Int): Double = n match {
      case 0 => y
      case _ => newTonSqrt(x, (y + x / y) / 2, n - 1)
    }
    newTonSqrt(x, 1, 10)
  }

  def sqrt2(x: Double): Double = {
    def isGoodEnough(r: Double): Boolean = math.abs(x - r * r) < 0.001
    def sqrtIter(r: Double): Double = {
      if(isGoodEnough(r)) r
      else sqrtIter((r + x / r) / 2)
    }
    sqrtIter(1)
  }

  def gcd(a: Int, b: Int): Int = {
    if(b == 0) a
    else gcd(b, a % b)
  }

}