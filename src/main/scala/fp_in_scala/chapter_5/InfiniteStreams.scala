package fp_in_scala.chapter_5

import scala.collection.immutable.Stream.cons


object InfiniteStreams {


  def constant[A](a: A): Stream[A] = {
    // cons(a, constant(a))
    unfold(a)(_ => Some(a, a))
  }


  def from(n: Int): Stream[Int] = {
    // cons(n, from(n + 1))
    unfold(n)(i => Some(i, i+1))
  }


  def fibs: Stream[Int] = {

//    def inner(a: Int, b: Int): Stream[Int] = cons(a, inner(b, a + b))
//
//    inner(0, 1)

    unfold((0, 1)) {
      case(a, b) => Some((a, (b, a + b)))
    }

  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Stream.empty
    }
  }

}
