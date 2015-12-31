package fp_in_scala.chapter_4


import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import fp_in_scala.chapter_4.Person._


@RunWith(classOf[JUnitRunner])
class PersonTests extends Specification {


  val nameErrorResult = MyLeft("Name is empty.")
  val ageErrorResult = MyLeft("Age is out of range.")


  "mkName" should {

    "return Right of name object when given valid string" in {
      mkName("Alex") must beEqualTo(MyRight(Name("Alex")))
    }

    "return Left of error string when given empty string" in {
      mkName("") must beEqualTo(nameErrorResult)
    }

    "return Left of error string when given null" in {
      mkName(null) must beEqualTo(nameErrorResult)
    }

  }


  "mkAge" should {

    "return Right of Age object when given valid (positive) age" in {
      mkAge(11) must beEqualTo(MyRight(Age(11)))
    }

    "return Right of Age object when given valid (zero) age" in {
      mkAge(0) must beEqualTo(MyRight(Age(0)))
    }

    "return Left of error string when given negative age" in {
      mkAge(-1) must beEqualTo(ageErrorResult)
    }

  }


  "mkPerson" should {

    "return Right of Person object when given valid name and age values" in {
      mkPerson("Bob", 22) must beEqualTo(MyRight(Person(Name("Bob"), Age(22))))
    }

    "return Left of name error string when only name is invalid" in {
      mkPerson("", 22) must beEqualTo(nameErrorResult)
    }

    "return Left of age error string when only age is invalid" in {
      mkPerson("Bob", -1) must beEqualTo(ageErrorResult)
    }

    "return Left of name error string when both arguments are invalid" in {
      mkPerson("", -1) must beEqualTo(nameErrorResult)
    }

  }


}
