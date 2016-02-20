package fp_in_scala.chapter_5

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import fp_in_scala.chapter_5.MyStream._


@RunWith(classOf[JUnitRunner])
class StreamUnfoldTests extends Specification {


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

      val actual = fibs
        .takeUsingUnfold(5)
        .toList

      actual should beEqualTo(List(0, 1, 1, 2, 3))

    }

  }


  "takeWhileUsingUnfold function" should {

    "correctly take elements of a stream while predicate is met" in {

      val actual = fibs
        .takeWhileUsingUnfold((i: Int) => i < 10)
        .toList

      actual should beEqualTo(List(0, 1, 1, 2, 3, 5, 8))

    }

  }


  "zipWithUsingUnfold function" should {

    val combiner = (i: Int, c: Char) => s"$i$c"

    val expected = List("1A", "2B", "3C", "4D", "5E")

    def testZipWithUnfold(stream1: MyStream[Int], stream2: MyStream[Char]) = {

      val actual = stream1
        .zipWithUsingUnfold(stream2, combiner)
        .toList

      actual should beEqualTo(expected)

    }

    "zip matching elements of two streams where both are the same length" in {

      testZipWithUnfold(
        MyStream(1, 2, 3, 4, 5),
        MyStream('A', 'B', 'C', 'D', 'E')
      )

    }

    "zip only matching elements of two streams where first is longer than second" in {

      testZipWithUnfold(
        MyStream(1, 2, 3, 4, 5, 6),
        MyStream('A', 'B', 'C', 'D', 'E')
      )

    }

    "zip only matching elements of two streams where first is shorter than second" in {

      testZipWithUnfold(
        MyStream(1, 2, 3, 4, 5),
        MyStream('A', 'B', 'C', 'D', 'E', 'F')
      )

    }

  }


  "zipAllUsingUnfold function" should {

    "zip all elements of two streams where both are the same length" in {

      val stream1 = MyStream(1, 2, 3)
      val stream2 = MyStream('A', 'B', 'C')

      val actual = stream1
        .zipAllUsingUnfold(stream2)
        .toList

      val expected = List(
        (Some(1), Some('A')),
        (Some(2), Some('B')),
        (Some(3), Some('C'))
      )

      actual should beEqualTo(expected)

    }

    "zip all elements of two streams where first is longer than second" in {

      val stream1 = MyStream(1, 2, 3, 4)
      val stream2 = MyStream('A', 'B', 'C')

      val actual = stream1
        .zipAllUsingUnfold(stream2)
        .toList

      val expected = List(
        (Some(1), Some('A')),
        (Some(2), Some('B')),
        (Some(3), Some('C')),
        (Some(4), None)
      )

      actual should beEqualTo(expected)

    }

    "zip all elements of two streams where first is shorter than second" in {

      val stream1 = MyStream(1, 2, 3)
      val stream2 = MyStream('A', 'B', 'C', 'D')

      val actual = stream1
        .zipAllUsingUnfold(stream2)
        .toList

      val expected = List(
        (Some(1), Some('A')),
        (Some(2), Some('B')),
        (Some(3), Some('C')),
        (None, Some('D'))
      )

      actual should beEqualTo(expected)

    }

  }

}