package fp_in_scala.chapter_6.RNG

object Rand {


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt


  def unit[A](a: A): Rand[A] =
    rng => (a, rng)


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

}
