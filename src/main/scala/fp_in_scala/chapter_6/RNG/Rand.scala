package fp_in_scala.chapter_6.RNG

object Rand {


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt


  def unit[A](a: A): Rand[A] =
    rng => (a, rng)


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(
      a =>
        rng =>
          (f(a), rng)
    )


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(
      a =>
        rng1 => {
          val (b, rng2) = rb(rng1)
          (f(a, b), rng2)
        }
    )
  }


  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldLeft((List.empty[A], rng))(
        (previous, fn) =>
          previous match {
            case (list, currentRNG) =>
              Rand.map(fn)(_ :: list)(currentRNG)
          }
      )
    }
  }


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, rng2) = f(rng)
      g(a)(rng2)
  }


}
