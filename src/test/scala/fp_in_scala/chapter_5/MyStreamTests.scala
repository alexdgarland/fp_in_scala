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


  def isEven(i: Int) = i % 2 == 0


  "takeWhile method" should {

    "take correct items where condition is met by some items" in {
      val result = MyStream(2, 4, 6, 7, 8, 10).takeWhile(isEven)
      result.toList should beEqualTo(List(2, 4, 6))
    }

    "return stream of all available elements where all meet condition" in {
      val result = MyStream(2, 4, 6, 8, 10).takeWhile(isEven)
      result.toList should beEqualTo(List(2, 4, 6, 8, 10))
    }

    "return empty stream where first element does not match condition" in {
      val result = MyStream(1, 4, 6, 7, 8, 10).takeWhile(isEven)
      result should be(MyStream.empty)
    }

    "return empty stream where original stream is empty" in {
      val result = MyStream.empty.takeWhile(isEven)
      result should be(MyStream.empty)
    }

  }


  "forAll method" should {

    "return true when all elements match" in {
      MyStream(2, 4, 6, 8, 10).forAll(isEven) should beTrue
    }

    "return false when all elements do not match" in {
      MyStream(1, 3, 5, 7, 9).forAll(isEven) should beFalse
    }

    "return false when a single element does not match" in {
      MyStream(2, 4, 7, 8, 10).forAll(isEven) should beFalse
    }

    "return true when calle on an empty list" in {
      MyStream.empty.forAll(isEven) should beTrue
    }

  }


  "takeWhileUsingFoldRight method" should {

    def isEven(i: Int) = i % 2 == 0

    "take correct items where condition is met by some items" in {
      val result = MyStream(2, 4, 6, 7, 8, 10).takeWhileUsingFoldRight(isEven)
      result.toList should beEqualTo(List(2, 4, 6))
    }

    "return stream of all available elements where all meet condition" in {
      val result = MyStream(2, 4, 6, 8, 10).takeWhileUsingFoldRight(isEven)
      result.toList should beEqualTo(List(2, 4, 6, 8, 10))
    }

    "return empty stream where first element does not match condition" in {
      val result = MyStream(1, 4, 6, 7, 8, 10).takeWhileUsingFoldRight(isEven)
      result should be(MyStream.empty)
    }

    "return empty stream where original stream is empty" in {
      val result = MyStream.empty.takeWhileUsingFoldRight(isEven)
      result should be(MyStream.empty)
    }

  }


  "headOptionUsingFoldRight method" should {

    "return Some of head when list has multiple elements" in {
      MyStream(1, 2, 3).headOptionUsingFoldRight should beEqualTo(Some(1))
    }

    "return Some of sole element when list has only one element" in {
      MyStream(1).headOptionUsingFoldRight should beEqualTo(Some(1))
    }

    "return None when list is empty" in {
      MyStream.empty.headOptionUsingFoldRight should beNone
    }

  }


  "map method" should {

    def double(i: Int) = i * 2

    "map function over stream" in {
      MyStream(1, 2, 3).map(double).toList should beEqualTo(List(2, 4, 6))
    }

    "return empty stream when called on empty stream" in {
      MyStream.empty.map(double) should be(MyStream.empty)
    }

  }

}
