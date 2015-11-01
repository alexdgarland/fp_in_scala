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

}
