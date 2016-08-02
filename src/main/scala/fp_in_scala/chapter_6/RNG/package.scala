package fp_in_scala.chapter_6

import fp_in_scala.chapter_6.RNG.Rand._


package object RNG {


  def nonNegativeInt: Rand[Int] =
    Rand.map(_.nextInt)(i => if (i == Int.MinValue) 0 else Math.abs(i))


  def double: Rand[Double] =
    Rand.map(nonNegativeInt)(i => Math.abs(i.toDouble / Int.MinValue.toDouble))


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


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    (1 to count).foldLeft((List.empty[Int], rng))(
      (previous: (List[Int], RNG), _) =>
        previous match {
          case (list, currentRNG) =>
            val (nextInt, nextRNG) = currentRNG.nextInt
            (nextInt :: list, nextRNG)
        }
    )

  }

}
