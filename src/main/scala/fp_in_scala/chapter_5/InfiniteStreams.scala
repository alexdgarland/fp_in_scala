package fp_in_scala.chapter_5

import scala.collection.immutable.Stream.cons


object InfiniteStreams {


  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

}
