package fp_in_scala.chapter_4

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import MyEither._


@RunWith(classOf[JUnitRunner])
class MyEitherTests extends Specification {


  val receiverRightVal = MyRight(2)
  val receiverLeftMsg = "Receiver was MyLeft"
  val receiverLeftVal = MyLeft(receiverLeftMsg)
  val tripleFunc = (i: Int) => i * 3
  val functionRightResult = MyRight(6)


  "map on MyEither" should {

    "return result of function when receiver is MyRight" in {
      receiverRightVal.map(tripleFunc) must beEqualTo(functionRightResult)
    }

    "return MyLeft of receiver when receiver is MyLeft" in {
      receiverLeftVal.map(tripleFunc) must beEqualTo(receiverLeftVal)
    }

  }


  "flatMap on MyEither" should {

    val tripleFuncAsMyRight = (i: Int) => MyRight(i * 3)
    val functionLeftVal = MyLeft("Function gives MyLeft value")
    val funcGivingMyLeft = (i: Int) => functionLeftVal

    "return result of function when receiver and output of function are both MyRight" in {
      receiverRightVal.flatMap(tripleFuncAsMyRight) must beEqualTo(MyRight(6))
    }

    "return MyLeft of receiver when receiver is MyLeft and output of function is MyRight" in {
      receiverLeftVal.flatMap(tripleFuncAsMyRight) must beEqualTo(receiverLeftVal)
    }

    "return MyLeft of function when receiver is MyRight and output of function is MyLeft" in {
      receiverRightVal.flatMap(funcGivingMyLeft) must beEqualTo(functionLeftVal)
    }

    "return MyLeft of receiver when receiver and output of function are both MyLeft" in {
      receiverLeftVal.flatMap(funcGivingMyLeft) must beEqualTo(receiverLeftVal)
    }

  }


  "orElse on MyEither" should {

    val rightDefault = MyRight(100)
    val leftDefault = MyLeft("default as MyLeft")

    "return original value where receiver is MyRight" in {
      receiverRightVal.orElse(rightDefault) must beEqualTo(receiverRightVal)
    }

    "return supplied default where receiver is MyLeft and default returns MyRight" in {
      receiverLeftVal.orElse(rightDefault) must beEqualTo(rightDefault)
    }

    "return supplied default where receiver is MyLeft and default returns MyLeft" in {
      receiverLeftVal.orElse(leftDefault) must beEqualTo(leftDefault)
    }

  }


  "map2 on MyEither" should {

    val multiplyIntAndFloatToString = (a: Int, b: Double) => s"Result is : ${a * b}"
    val argumentRightVal = MyRight(3.0)
    val map2FunctionResult = MyRight("Result is : 6.0")
    val argumentLeftMsg = "Argument was MyLeft"
    val argumentLeftVal = MyLeft(argumentLeftMsg)

    "return result of function when receiver and argument are both MyRight" in {
      receiverRightVal.map2(argumentRightVal)(multiplyIntAndFloatToString) must beEqualTo(map2FunctionResult)
    }

    "return MyLeft of list containing MyLeft of receiver when receiver is MyLeft and argument is MyRight" in {
      receiverLeftVal.map2(argumentRightVal)(multiplyIntAndFloatToString) must beEqualTo(MyLeft(List(receiverLeftMsg)))
    }

    "return MyLeft of list containing MyLeft of argument when receiver is MyRight and argument is MyLeft" in {
      receiverRightVal.map2(argumentLeftVal)(multiplyIntAndFloatToString) must beEqualTo(MyLeft(List(argumentLeftMsg)))
    }

    "return MyLeft of list containing both MyLeft values when receiver and argument are both MyLeft" in {
      receiverLeftVal.map2(argumentLeftVal)(multiplyIntAndFloatToString) must beEqualTo(
        MyLeft(List(receiverLeftMsg, argumentLeftMsg))
      )
    }

  }


  "sequence over list of MyEither" should {

    "return MyRight[List] where all list elements are MyRight" in {
      sequence(List(MyRight(1), MyRight(2), MyRight(3))) must beEqualTo(MyRight(List(1, 2, 3)))
    }

    "return list of all MyLeft results where not all list elements are MyRight" in {
      val inputList = List(MyRight(1), MyLeft("Error 1"), MyLeft("Error 2"), MyRight(2))
      sequence(inputList) must beEqualTo(MyLeft(List("Error 1", "Error 2")))
    }

  }


  "traverse over list of MyEither" should {

    val doubleToRightIfEven = (i: Int) => if (i % 2 == 0) MyRight(i * 2) else MyLeft(s"Fail at value : $i")

    "return MyRight[List[B]] where all list elements can be converted to MyRight[B] by given function" in {
      traverse(List(2, 4, 6))(doubleToRightIfEven) must beEqualTo(MyRight(List(4, 8, 12)))
    }

    "return list of all MyLeft results where not all list elements can be converted to MyRight[B] by given function" in {
      val expectedLeft = MyLeft(List("Fail at value : 5", "Fail at value : 7"))
      traverse(List(2, 5, 6, 7))(doubleToRightIfEven) must beEqualTo(expectedLeft)
    }

  }


}
