package fp_in_scala.chapter_5

import scala.collection.immutable.Stream.cons


object InfiniteStreams {


  def constant[A](a: A): Stream[A] = {
    // cons(a, constant(a))
    unfold(a)(_ => Some(a, a))
  }


  def from(n: Int): Stream[Int] = {
    // cons(n, from(n + 1))
    unfold(n)(i => Some(i, i + 1))
  }


  def fibs: Stream[Int] = {

    //    def inner(a: Int, b: Int): Stream[Int] = cons(a, inner(b, a + b))
    //
    //    inner(0, 1)

    unfold((0, 1)) {
      case (a, b) => Some((a, (b, a + b)))
    }

  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Stream.empty
    }
  }


  implicit class StreamExtensions[A](s: Stream[A]) {


    def mapUsingUnfold[B](f: A => B): Stream[B] = {
      unfold(s)((str: Stream[A]) => Some(f(str.head), str.tail))
    }


    def takeUsingUnfold(n: Int): Stream[A] = {
      unfold((s, n)) {
        case (str: Stream[A], i: Int) =>
          if (i < 1) None
          else Some((str.head, (str.tail, i - 1)))
      }
    }


    def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = {
      unfold((s)) {
        (str: Stream[A]) => {
          val h = str.head
          if (p(h)) Some((h, str.tail))
          else None
        }
      }
    }


  }


}
