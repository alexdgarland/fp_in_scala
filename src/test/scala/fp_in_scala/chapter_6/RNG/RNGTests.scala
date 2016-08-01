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
      val (generatedDouble, _) = double(ConstantRNG(Int.MaxValue))
      generatedDouble must beLessThan(1.0)
      (1.0 - generatedDouble) must beLessThan(0.000001)
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


  "intDouble function" should {

    "call the RNG twice" in {
      val (_, newRNG) = intDouble(ListRNG(List(1, 2, 3)))
      val(nextGeneratedInt, _) = newRNG.nextInt
      nextGeneratedInt must beEqualTo(3)
    }

  }

  "doubleInt function" should {

    "call the RNG twice" in {
      val (_, newRNG) = doubleInt(ListRNG(List(1, 2, 3)))
      val(nextGeneratedInt, _) = newRNG.nextInt
      nextGeneratedInt must beEqualTo(3)
    }

  }

  "double3 function" should {

    "return 3 different doubles in a single call" in {
      val (doubles, _) = double3(ListRNG(List(1, 2, 3, 4)))
      doubles.productIterator.toSet.size must beEqualTo(3)
    }

    "call the RNG three times" in {
      val (_, newRNG) = double3(ListRNG(List(1, 2, 3, 4)))
      val(nextGeneratedInt, _) = newRNG.nextInt
      nextGeneratedInt must beEqualTo(4)
    }

  }


  "ints function" should {

    "return as many ints as requested" in {
      val (results, _) = ints(5)(ConstantRNG(1))
      results.size must beEqualTo(5)
    }

    "return several different int values" in {
      val (results, _) = ints(10)(ListRNG((1 to 10).toList))
      results.toSet.size must beGreaterThan(5)
    }

    "call the RNG as many times as the count requested" in {
      val (_, newRNG) = ints(3)(ListRNG(List(1, 2, 3, 4)))
      val (nextGeneratedInt, _) = newRNG.nextInt
      nextGeneratedInt must beEqualTo(4)
    }

  }

}
