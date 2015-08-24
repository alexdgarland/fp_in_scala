package fp_in_scala

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class Chapter2Tests extends Specification {


  "Factorial function as implemented in the book" should {
    "return 6 for input 3" in { factorial(3) must equalTo(6) }
    "return 120 for input 5" in { factorial(5) must equalTo(120) }
  }

  "Factorial without inner function" should {
    "return 6 for input 3 without using inner function" in { factorial_no_inner_func(3) must equalTo(6) }
    "return 120 for input 5 without using inner function" in { factorial_no_inner_func(5) must equalTo(120) }
  }

  "Tailrec factorial without inner function" should {
    "return 6 for input 3, no inner func, extra param" in { factorial_no_inner_func_2(3) must equalTo(6) }
    "return 120 for input 5, no inner func, extra param" in { factorial_no_inner_func_2(5) must equalTo(120) }
  }

  "Non-tail-recursive Fibonacci function" should {
    "give first Fibonacci number as 0 (non-tail-rec)" in { fib_nontailrec(1) must equalTo(0) }
    "give second Fibonacci number as 1 (non-tail-rec)" in { fib_nontailrec(2) must equalTo(1) }
    "give third Fibonacci number as 1 (non-tail-rec)" in { fib_nontailrec(3) must equalTo(1) }
    "give fourth Fibonacci number as 2 (non-tail-rec)" in { fib_nontailrec(4) must equalTo(2) }
    "give fifth Fibonacci number as 3 (non-tail-rec)" in { fib_nontailrec(5) must equalTo(3) }
    "give sixth Fibonacci number as 5 (non-tail-rec)" in { fib_nontailrec(6) must equalTo(5) }
  }

  "Tail-recursive Fibonacci function" should {
    "give first Fibonacci number as 0" in { fib(1) must equalTo(0) }
    "give second Fibonacci number as 1" in { fib(2) must equalTo(1) }
    "give third Fibonacci number as 1" in { fib(3) must equalTo(1) }
    "give fourth Fibonacci number as 2" in { fib(4) must equalTo(2) }
    "give fifth Fibonacci number as 3" in { fib(5) must equalTo(3) }
    "give sixth Fibonacci number as 5" in { fib(6) must equalTo(5) }
  }

  "IsSorted function"  should {

    "with ascending numerical sort checker (non-strict)" in  {

      def nonStrictSort (a : Int, b : Int) = (a <= b)

      "report empty array as non-strictly-sorted" in {
        val testArray = Array.empty[Int]
        isSorted(testArray, nonStrictSort) must beTrue
      }

      "report array with single element as non-strictly-sorted" in {
        val testArray = Array(4)
        isSorted(testArray, nonStrictSort) must beTrue
      }

      "report numerically-sorted array as non-strictly-sorted" in {
        val testArray = Array(1, 2, 3, 4)
        isSorted(testArray, nonStrictSort) must beTrue
      }

      "report numerically-reverse-sorted array as not non-strictly-sorted" in {
        val testArray = Array(4, 3, 2, 1)
        isSorted(testArray, nonStrictSort) must beFalse
      }

      "report homogeneous array as non-strictly-sorted" in {
        val testArray = Array(4, 4, 4, 4)
        isSorted(testArray, nonStrictSort) must beTrue
      }

    }

    "with ascending numerical sort checker (strict)" in {

      def strictSort (a : Int, b : Int) = (a < b)

      "report empty array as strictly-sorted" in {
        val testArray = Array.empty[Int]
        isSorted(testArray, strictSort) must beTrue
      }

      "report array with single element as strictly-sorted" in {
        val testArray = Array(4)
        isSorted(testArray, strictSort) must beTrue
      }

      "report numerically-sorted array as strictly-sorted" in {
        val testArray = Array(1, 2, 3, 4)
        isSorted(testArray, strictSort) must beTrue
      }

      "report numerically-reverse-sorted array as not strictly-sorted" in {
        val testArray = Array(4, 3, 2, 1)
        isSorted(testArray, strictSort) must beFalse
      }

      "report homogeneous array as not strictly-sorted" in {
        val testArray = Array(4, 4, 4, 4)
        isSorted(testArray, strictSort) must beFalse
      }

    }

    "with sort on third character of string (custom)" in {

      def customSort(a : String, b : String) : Boolean = {
        a.charAt(2) < b.charAt(2)
      }

      "report empty array as custom-sorted" in {
        val testArray = Array.empty[String]
        isSorted(testArray, customSort) must beTrue
      }

      "report array with single element as custom-sorted" in {
        val testArray = Array("Two")
        isSorted(testArray, customSort) must beTrue
      }

      "report custom-sorted array as custom-sorted" in {
        val testArray = Array("THREE", "One", "Two", "Four")
        isSorted(testArray, customSort) must beTrue
      }

      "report non-custom-sorted array as not custom-sorted" in {
        val testArray = Array("One", "Two", "THREE", "Four")
        isSorted(testArray, customSort) must beFalse
      }

    }
  }

}
