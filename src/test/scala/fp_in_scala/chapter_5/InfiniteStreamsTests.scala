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

}
