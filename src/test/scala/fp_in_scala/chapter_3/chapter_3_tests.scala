package fp_in_scala.chapter_3

import fp_in_scala.chapter_3.functions._
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class Chapter3Tests extends Specification {

  "Tail function" should {

    "return empty list when given empty list" in {
      tail(List.empty[Int]) must beEqualTo(Nil)
    }

    "return empty list when given list with one item" in {
      tail(List(1)) must beEqualTo(Nil)
    }

    "return list of length 1 containing second item when given list with two items" in {
      val result = tail(List(1, 2))
      result must beEqualTo(List(2))
    }

    "return remainder of list after head when given list with multiple items" in {
      val result = tail(List(1, 2, 3, 4, 5))
      result must beEqualTo(List(2, 3, 4, 5))
    }

  }

  "SetHead function" should {

    // Not clear what behaviour here should be,
    // so let's just pick something and test for it.
    "return empty list (no head) when given empty list" in {
      sethead(List.empty[Int], 1) must beEqualTo(Nil)
    }

    "return new head value in list when given list with one item" in {
      tail(List(1)) must beEqualTo(Nil)
    }

    "return list with new head plus second existing item when given list with two items" in {
      val originalList = List(1, 2)
      val result = sethead(originalList, 3)
      result must beEqualTo(List(3, 2))
    }

    "return list with new head plus tail of original list when given list with multiple items" in {
      val originalList = List(1, 2, 3, 4, 5)
      val result = sethead(originalList, 10)
      result must beEqualTo(List(10, 2, 3, 4, 5))
    }

  }


  "Drop function" should {

    "return empty list when asked to drop from empty list" in {
      drop(List.empty[Int], 1) must beEqualTo(Nil)
    }

    "return empty list when asked to drop more elements than present in list" in {
      drop(List(1, 2), 3) must beEqualTo(Nil)
    }

    "return empty list when asked to drop all elements present in list" in {
      drop(List(1, 2), 2) must beEqualTo(Nil)
    }

    "return correct elements when asked to drop less than the full list" in {
      drop(List(1, 2, 3, 4, 5, 6), 3) must beEqualTo(List(4, 5, 6))
    }

    "return original list when asked to drop zero elements" in {
      val originalList = List(1, 2, 3, 4, 5, 6)
      drop(originalList, 0) must beEqualTo(originalList)
    }

  }


  "Drop While function" should {

    def greaterThan10 = (i : Int) => i > 10

    "return empty list when given empty list (regardless of predicate)" in {
      dropwhile(List.empty[Int], greaterThan10) must beEqualTo(Nil)
    }

    "return empty list when all elements match predicate" in {
      dropwhile(List(11, 12), greaterThan10) must beEqualTo(Nil)
    }

    "return original list when no elements match predicate" in {
      val originalList = List(1, 2, 3)
      dropwhile(originalList, greaterThan10) must beEqualTo(originalList)
    }

    "return list starting from first non-matchig element when some match predicate" in {
      dropwhile(List(11, 22, 2, 3, 33), greaterThan10) must beEqualTo(List(2, 3, 33))
    }

  }


  "Init function" should {

    "return empty list as init of empty list " in {
      init(Nil) must beEqualTo(Nil)
    }

    "return empty list as init of list with single element" in {
      init(List(1)) must beEqualTo(Nil)
    }

    "return all but last element as init of list with multiple elements" in {
      init(List(1, 2, 3, 4)) must beEqualTo(List(1, 2, 3))
    }

  }


  "Length function implemented with foldRight" should {

    "return zero for empty list" in {
      lengthWithFoldRight(List.empty[Int]) must beEqualTo(0)
    }

    "return two when list size is two" in {
      lengthWithFoldRight(List(1, 2)) must beEqualTo(2)
    }

    "return five when list size is five" in {
      lengthWithFoldRight(List(1, 2, 1, 2, 1)) must beEqualTo(5)
    }

  }


  "foldLeft" should {

    val testList = List(2, 4, 5)

    "perform left-associative division over a list when implemented iteratively" in {
      iterFoldLeft(testList,1000)((acc,next)=>acc/next) must beEqualTo(25)
    }

    "perform left-associative division over a list when implemented tail-recursively" in {
      recFoldLeft(testList,1000)((acc,next)=>acc/next) must beEqualTo(25)
    }

    "allow us to sum a list" in {
      sumWithFoldLeft(List(1,2,3,4,5)) must beEqualTo(15)
    }

    "allow us to find the product of a list" in {
      productWithFoldLeft(List(1,2,3,4,5)) must beEqualTo(120)
    }

    "allow us to find the length of a list" in {
      lengthWithFoldLeft(List(1,2,3,4,5)) must beEqualTo(5)
    }

    "allow us to reverse a list" in {
      reverseWithFoldLeft(List(1,2,3,4,5)) must beEqualTo(List(5,4,3,2,1))
    }

  }


  "append" should {

    "be implementable with foldRight" in {
      appendWithFoldRight(List(1,2,3), 4) must beEqualTo(List(1,2,3,4))
    }

  }

}
