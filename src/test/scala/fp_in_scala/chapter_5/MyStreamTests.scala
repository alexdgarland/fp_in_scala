package fp_in_scala.chapter_5

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class MyStreamTests extends Specification {


  def streamOf3 = MyStream(1, 2, 3)


  def listOf3 = List(1, 2, 3)


  "toList method" should {

    "return empty list whif (n > 0)en stream is empty" in {
      MyStream.empty.toList should be(List.empty)
    }

    "return correct list when stream is not empty" in {
      streamOf3.toList should beEqualTo(listOf3)
    }

  }


  "take method" should {

    "take first n items in stream when n < length" in {
      val resultAsList = MyStream(1, 2, 3, 4, 5).take(3).toList
      resultAsList should beEqualTo(List(1, 2, 3))
    }

    "return stream of all available elements when n == length" in {
      streamOf3.take(3).toList should beEqualTo(listOf3)
    }

    "return stream of all available elements when n > length" in {
      streamOf3.take(4).toList should beEqualTo(listOf3)
    }

  }


  "drop method" should {

    "drop first n items in stream when n < length" in {
      val resultAsList = MyStream(1, 2, 3, 4, 5).drop(3).toList
      resultAsList should beEqualTo(List(4, 5))
    }

    "return empty stream when n == length" in {
      streamOf3.drop(3) should be(MyEmpty)
    }

    "return empty stream when n > length" in {
      streamOf3.drop(4) should be(MyStream.empty)
    }

  }

}
