package fp_in_scala.chapter_6

import fp_in_scala.chapter_6.RNG.Rand._


package object RNG {


  def nonNegativeInt: Rand[Int] =
    Rand.map(_.nextInt)(i => if (i == Int.MinValue) 0 else Math.abs(i))


  def double: Rand[Double] =
    Rand.map(nonNegativeInt)(i => Math.abs(i.toDouble / Int.MinValue.toDouble))


  def intDouble: Rand[(Int, Double)] =
    both(_.nextInt, double)


  def doubleInt: Rand[(Double, Int)] =
    both(double, _.nextInt)


  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleValue1, rng1) = double(rng)
    val (doubleValue2, rng2) = double(rng1)
    val (doubleValue3, rng3) = double(rng2)
    ((doubleValue1, doubleValue2, doubleValue3), rng3)
  }


  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)((rng: RNG) => rng.nextInt))(_)


  def nonNegativeIntLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(
      i => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          rng => (mod, rng)
        else
          nonNegativeIntLessThan(n)
      }
    )
  }

}
