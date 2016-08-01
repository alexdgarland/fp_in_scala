package fp_in_scala.chapter_6.RNG

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class RNGTests extends Specification {


  // Implementation of RNG which always returns a set value - for testing
  case class ConstantRNG(intConstant: Int) extends RNG {

    override def nextInt: (Int, RNG) = {
      (intConstant, this)
    }

  }

  // Implementation of RNG which iterates through a pre-determined list of integer values - for testing
  case class ListRNG(intList: List[Int]) extends RNG {

    override def nextInt: (Int, RNG) = {

      if (intList.isEmpty) {
        // Okay not to use a side-effecting Exception here
        // as it's just to alert us to failure of an implicit test expectation.
        throw new RuntimeException("Fixed list of deterministic values for test RNG exhausted.")
      }

      (intList.head, ListRNG(intList.tail))
    }

  }


  "nonNegativeInt function" should {

    "return original value from RNG when it is zero" in {
      val (generatedInt, _) = nonNegativeInt(ConstantRNG(0))
      generatedInt must beEqualTo(0)
    }

    "return original value from RNG when it is positive" in {
      val (generatedInt, _) = nonNegativeInt(ConstantRNG(10356))
      generatedInt must beEqualTo(10356)
    }

    "return positive of original value from RNG when it is negative" in {
      val (generatedInt, _) = nonNegativeInt(ConstantRNG(-10356))
      generatedInt must beEqualTo(10356)
    }

    "return zero when the original value from the RNG is Int.MinValue" in {
      val (generatedInt, _) = nonNegativeInt(ConstantRNG(Int.MinValue))
      generatedInt must beEqualTo(0)
    }

    "return next iteration of RNG unaltered" in {
      val (generatedInt, newRNG) = nonNegativeInt(ListRNG(List(3, 7)))
      generatedInt must beEqualTo(3)
      val (nextGeneratedInt, _) = newRNG.nextInt
      nextGeneratedInt must beEqualTo(7)
    }

  }

  "double function" should {

    "return zero when RNG yields zero" in {
      val (generatedDouble, _) = double(ConstantRNG(0))
      generatedDouble must beEqualTo(0.0)
    }

    "return value almost but not quite one when RNG yields Int.MaxValue" in {
      val (generatedDouble, _) = double(ConstantRNG(0))
      generatedDouble must beEqualTo(0.0)
    }

    "return same value for positive and negative of same RNG value" in {
      val (generatedDouble1, _) = double(ConstantRNG(1350))
      val (generatedDouble2, _) = double(ConstantRNG(-1350))
      generatedDouble1 must beEqualTo(generatedDouble2)
    }

    "return next iteration of RNG unaltered" in {
      val (generatedDouble, newRNG) = double(ListRNG(List(0, 7)))
      generatedDouble must beEqualTo(0.0)
      val (nextGeneratedInt, _) = newRNG.nextInt
      nextGeneratedInt must beEqualTo(7)
    }

  }


}
