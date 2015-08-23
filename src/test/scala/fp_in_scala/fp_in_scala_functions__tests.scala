package fp_in_scala

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class FunctionTests extends Specification {


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

}
