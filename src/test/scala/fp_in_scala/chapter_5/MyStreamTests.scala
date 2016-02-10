package fp_in_scala.chapter_5

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class MyStreamTests extends Specification {


  "toList method" should {

    "return empty list when stream is empty" in {
      MyStream.empty.toList should be(List.empty)
    }

    "return correct list when stream is not empty" in {
      MyStream(1, 2, 3).toList should beEqualTo(List(1, 2, 3))
    }

  }

}
