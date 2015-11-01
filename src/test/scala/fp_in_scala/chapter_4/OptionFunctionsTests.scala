package fp_in_scala.chapter_4

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import fp_in_scala.chapter_4.OptionFunctions._


@RunWith(classOf[JUnitRunner])
class OptionFunctionsTests extends Specification {

  "map2 function" should {

    // Define integer addition with explicit typing
    // to force functions passed None to infer properly.
    val intAdd = (a:Int,b:Int)=>a+b

    "return None when first argument is None" in {
      map2(None, Some(2))(intAdd) must beEqualTo(None)
    }

    "return None when second argument is None" in {
      map2(Some(1), None)(intAdd) must beEqualTo(None)
    }

    "return None when both arguments are None" in {
      map2(None, None)(intAdd) must beEqualTo(None)
    }

    "return result of function when neither argument is None" in {
      map2(Some(1), Some(2))(intAdd) must beEqualTo(Some(3))
    }

  }

  "sequence function" should {

    "return list of values where original list contains no Nones" in {
      sequence(List(Some(1), Some(2), Some(3))) must beEqualTo(Some(List(1,2,3)))
    }

    "return empty list where original list is empty" in {
      sequence(List()) must beEqualTo(Some(List()))
    }

    "return None where original list is all Nones" in {
      sequence(List(None, None, None)) must beEqualTo(None)
    }

    "return None where original list starts with None" in {
      sequence(List(None, Some(2), Some(3))) must beEqualTo(None)
    }

    "return None where original list ends with None" in {
      sequence(List(Some(1), Some(2), None)) must beEqualTo(None)
    }

    "return None where original list has None in the middle" in {
      sequence(List(Some(1), None, Some(3))) must beEqualTo(None)
    }

  }

}
