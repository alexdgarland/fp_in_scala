package fp_in_scala.chapter_2

/**
  *
  * Functions implemented to complete exercises and generally explore concepts from
  * Chiusano, Bjarnason - "Functional Programming in Scala"
  *
  */

object functions {


  /*

  Factorial functions

   */

  // This is as implemented in the book:
  def factorial(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }


  // This is functionally the same, but does not use tail recursion.
  def factorial_no_inner_func(n: Int): Int = {
    if (n <= 0) 1
    else n * factorial_no_inner_func(n - 1)
  }


  // This avoids having an inner function while maintaining tail recursion,
  // but at the cost of having an extra (defaulted) parameter,
  // which makes the function signature less clear and open to misuse.
  // Therefore, we can see why having an inner function is useful.
  @annotation.tailrec
  def factorial_no_inner_func_2(n: Int, acc: Int = 1): Int = {
    if (n <= 0) acc
    else factorial_no_inner_func_2(n - 1, n * acc)
  }


  /*

  2.1 - Fibonacci functions

  Note that these follow the given definition of a Fibonacci sequence as starting (0, 1, ...).

   */

  // To get started - a simple, non-tail-recursive version.
  def fib_nontailrec(n: Int): Int = {
    if (n <= 1) 0
    else if (n == 2) 1
    else fib_nontailrec(n - 1) + fib_nontailrec(n - 2)
  }


  // "Proper" tail-recursive solution.
  def fib(n: Int): Int = {

    @annotation.tailrec
    def inner(first: Int, second: Int, n: Int): Int = {
      if (n <= 1) first
      else inner(second, first + second, n - 1)
    }

    inner(0, 1, n)
  }


  // Impression so far is that although tail-call elimination is a useful optimisation,
  // using it can lose some of the clarity of the simplest recursive solutions.


  /*

  2.2 - isSorted

  */

  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) true
    else if (!ordered(as(0), as(1))) false
    else isSorted(as.tail, ordered)
  }


  /*

  2.3 - Curry

  */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }


  /*

  2.4 - Uncurry

  */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }


  /*

  2.5 - Compose

  */

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
