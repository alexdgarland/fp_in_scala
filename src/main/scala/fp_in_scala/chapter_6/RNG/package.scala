package fp_in_scala.chapter_6

package object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rawInt, newRNG) = rng.nextInt
    val nonNegIntValue = if(rawInt == Int.MinValue) 0 else Math.abs(rawInt)
    (nonNegIntValue, newRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nonNegInt, newRNG) = nonNegativeInt(rng)
    val randomDouble = Math.abs(nonNegInt.toDouble / Int.MinValue.toDouble)
    (randomDouble, newRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intValue, nextRNG) = rng.nextInt
    val (doubleValue, finalRNG) = double(nextRNG)
    ((intValue, doubleValue), finalRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (doubleValue, nextRNG) = double(rng)
    val (intValue, finalRNG) = nextRNG.nextInt
    ((doubleValue, intValue), finalRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleValue1, rng1) = double(rng)
    val (doubleValue2, rng2) = double(rng1)
    val (doubleValue3, rng3) = double(rng2)
    ((doubleValue1, doubleValue2, doubleValue3), rng3)
  }

}
