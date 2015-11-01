package fp_in_scala.chapter_4

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._
import VarianceWithOption._

@RunWith(classOf[JUnitRunner])
class VarianceWithOptionTests extends Specification {

  "meanOfSeq function" should {

    "return None as mean of empty sequence" in {
      meanOfSeq(Seq()) must beEqualTo(MyNone())
    }

    "return correct value for sequence of values" in {
      meanOfSeq(Seq(1.0, 2.0, 3.0, 4.0)) must beEqualTo(MySome(2.5))
    }

  }

  "calculateVariance function" should {

    "return None as variance of empty sequence" in {
      calculateVariance(Seq()) must beEqualTo(MyNone())
    }

    "calculate correct variance for sequence of integers" in {
      val inputList = Seq(1.0, 5.0, 23.0, 100.0)
      calculateVariance(inputList) must beEqualTo(MySome(1598.6875))
    }

  }

}



// sum = 129
// mean = 129 / 4 = 32.25
// diffs = -31.25, -27.25, -9.25, +67.75
// squared diffs = 976.5625, 742.5625, 85.5625, 4590.0625
// sum of squared diffs = 6394.75
// mean of squared diffs (AKA variance) = 1598.6875
