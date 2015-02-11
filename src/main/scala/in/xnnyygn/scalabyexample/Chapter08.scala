package in.xnnyygn.scalabyexample

object Chapter08 {

  abstract class Stack[+A] {
    def isEmpty: Boolean
    def push[B >: A](x: B): Stack[B] = new NonEmptyStack(x, this)
    def pop: (A, Stack[A])
  }

  object EmptyStack extends Stack[Nothing] {
    def isEmpty: Boolean = true
    def pop = sys.error("no element")
  }

  class NonEmptyStack[+A](top: A, rest: Stack[A]) extends Stack[A] {
    def isEmpty: Boolean = false
    def pop: (A, Stack[A]) = (top, rest)
  }

}