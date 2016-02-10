package fp_in_scala.chapter_4

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._


@RunWith(classOf[JUnitRunner])
class MyOptionTests extends Specification {


  "map on Option" should {

    val times2 = (n: Int) => n * 2

    "return result of function when applied to Some" in {
      MySome(10).map(times2) must beEqualTo(MySome(20))
    }

    "return None when applied to None (map)" in {
      MyNone().map(times2) must beEqualTo(MyNone())
    }

  }


  "flatMap on Option" should {

    val optionalTimes2 = (n: Int) => if (n < 10) MySome(n * 2) else MyNone()

    "return result of function when it is applied to Some and returns Some" in {
      MySome(5).flatMap(optionalTimes2) must beEqualTo(MySome(10))
    }

    "return result of function when it is applied to Some and returns None" in {
      MySome(10).flatMap(optionalTimes2) must beEqualTo(MyNone())
    }

    "return None when applied to None (flatMap)" in {
      MyNone().flatMap(optionalTimes2) must beEqualTo(MyNone())
    }

  }


  "getOrElse on Option" should {

    "return value when applied to Some" in {
      MySome(10).getOrElse(100) must beEqualTo(10)
    }

    "return default when applied to None" in {
      MyNone().getOrElse(100) must beEqualTo(100)
    }

  }


  "orElse on Option" should {

    "return the first Option when it is Some" in {
      MySome(10).orElse(MySome(100)) must beEqualTo(MySome(10))
    }

    "return second Option when first Option is None" in {
      MyNone().orElse(MySome(100)) must beEqualTo(MySome(100))
    }

  }


  "filter on Option" should {

    val ge10 = (n: Int) => n >= 10

    "return Some when applied to Some and function evaluates true" in {
      MySome(10).filter(ge10) must beEqualTo(MySome(10))
    }

    "return None when applied to Some and function evaluates false" in {
      MySome(5).filter(ge10) must beEqualTo(MyNone())
    }

    "return None when applied to None" in {
      MyNone().filter(ge10) must beEqualTo(MyNone())
    }

  }

}
