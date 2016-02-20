package fp_in_scala.chapter_5

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import fp_in_scala.chapter_5.InfiniteStreams._


@RunWith(classOf[JUnitRunner])
class InfiniteStreamsTests extends Specification {


  "constant function" should {

    "return ones when called with value one" in {

      val sample = constant(1)
        .take(5)
        .toList

      sample should beEqualTo(List(1, 1, 1, 1, 1))

    }

    "return fives when called with value one" in {

      val sample = constant(5)
        .take(5)
        .toList

      sample should beEqualTo(List(5, 5, 5, 5, 5))

    }

    "return strings when called with a string value" in {

      val sample = constant("Hello World")
        .take(3)
        .toList

      sample should beEqualTo(List("Hello World", "Hello World", "Hello World"))

    }

  }


  "from function" should {

    "return numbers one to ten as first ten elements when started from one" in {

      val sample = from(1)
        .take(10)
        .toList

      sample should beEqualTo(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

    }

    "return numbers fifty to fifty-nine as first ten elements when started from fifty" in {

      val sample = from(50)
        .take(10)
        .toList

      sample should beEqualTo(List(50, 51, 52, 53, 54, 55, 56, 57, 58, 59))

    }

  }


  "fibs function" should {

    "correctly return first ten elements of the Fibonacci sequence" in {

      val sample = fibs
        .take(10)
        .toList

      sample should beEqualTo(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))

    }

  }


  "mapUsingUnfold function" should {

    "correctly map function over the first ten elements of a stream" in {

      val sample = fibs
        .mapUsingUnfold((i: Int) => i * 2)
        .take(10)
        .toList

      sample should beEqualTo(List(0, 2, 2, 4, 6, 10, 16, 26, 42, 68))

    }

  }


  "takeUsingUnfold function" should {

    "correctly take first five elements of a stream" in {

      val sample = fibs
        .takeUsingUnfold(5)
        .toList

      sample should beEqualTo(List(0, 1, 1, 2, 3))

    }

  }

}