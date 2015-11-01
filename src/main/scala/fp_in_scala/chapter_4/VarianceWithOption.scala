package fp_in_scala.chapter_4

import Math.pow

// Solution to Exercise 4.2
// Mostly cribbed from here -
// https://github.com/astorije/fpinscala-exercises/blob/master/src/main/scala/ch4errorhandling/Option.scala
// to ensure I understood what I was doing - with my own added comments added.

object VarianceWithOption {

  // This function will return None if it "fails" (i.e is applied to an empty list)
  def meanOfSeq(sequence : Seq[Double]) : MyOption[Double] = {
    sequence match {
      case Seq() => MyNone()
      case l => MySome(l.sum / l.length)
    }
  }

  // Overall function to calculate variance.  We chain two uses of meanOfSeq, hence flatMap is useful.
  def calculateVariance (seq : Seq[Double]) : MyOption[Double] = {
    // Inner function explicitly capturing what we are passing to flatMap.
    def varianceGivenMean(mean : Double) = meanOfSeq(seq.map(elem => pow(elem - mean, 2)))
    // Apply using flatMap
    meanOfSeq(seq).flatMap(meanOfElements => varianceGivenMean(meanOfElements))
  }

}
