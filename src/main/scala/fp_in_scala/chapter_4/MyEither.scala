package fp_in_scala.chapter_4


sealed trait MyEither[+E, +A] {

  def map[B](f : A => B) : MyEither[E,B] = this match {
    case MyLeft(e) => MyLeft(e)
    case MyRight(a) => MyRight(f(a))
  }

  def flatMap[EE>:E,B](f : A => MyEither[EE,B]) : MyEither[EE,B] = this match {
    case MyLeft(e) => MyLeft(e)
    case MyRight(a) => f(a)
  }

  def orElse[EE>:E,B>:A](b : => MyEither[EE,B]) : MyEither[EE,B] = this match {
    case MyLeft(e) => b
    case MyRight(a) => MyRight(a)
  }

  def map2[EE>:E, B, C](b : MyEither[EE,B])(f : (A,B)=>C) : MyEither[EE,C] = {
    this.flatMap(a_val => b.map(b_val => f(a_val, b_val)))
  }

}


case class MyLeft[+E](value: E) extends MyEither[E, Nothing]


case class MyRight[+A](value: A) extends MyEither[Nothing, A]



