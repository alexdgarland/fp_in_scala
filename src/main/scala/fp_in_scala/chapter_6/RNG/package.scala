package fp_in_scala.chapter_6

package object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rawInt, newRNG) = rng.nextInt
    val nonNegIntValue = if(rawInt == Int.MinValue) 0 else Math.abs(rawInt)
    (nonNegIntValue, newRNG)
  }

}
