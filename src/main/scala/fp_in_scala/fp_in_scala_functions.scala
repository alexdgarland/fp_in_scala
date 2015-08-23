/**
 *
 * Functions implemented to complete exercises and generally explore concepts from
 * Chiusano, Bjarnason - "Functional Programming in Scala"
 *
 */

package object fp_in_scala {

  // This is as implemented in the book:
  def factorial(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <=0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // This is functionally the same, but does not use tail recursion.
  def factorial_no_inner_func(n: Int) : Int =
  {
    if (n <= 0) 1
    else n * factorial_no_inner_func(n-1)
  }

  // This avoids having an inner function while maintaining tail recursion,
  // but at the cost of having an extra (defaulted) parameter,
  // which makes the function signature less clear and open to misuse.
  // Therefore, we can see why having an inner function is useful.
  @annotation.tailrec
  def factorial_no_inner_func_2(n: Int, acc : Int = 1) : Int =
  {
    if (n <= 0) acc
    else factorial_no_inner_func_2(n-1, n * acc)
  }

}
