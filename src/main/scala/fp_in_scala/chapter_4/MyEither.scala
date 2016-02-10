package fp_in_scala.chapter_4


sealed trait MyEither[+E, +A] {


  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyLeft(e) => MyLeft(e)
    case MyRight(a) => MyRight(f(a))
  }


  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(e) => MyLeft(e)
    case MyRight(a) => f(a)
  }


  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(e) => b
    case MyRight(a) => MyRight(a)
  }


  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[List[EE], C] = (this, b) match {
    case (MyRight(a_val), MyRight(b_val)) => MyRight(f(a_val, b_val))
    case (MyLeft(a_err), MyRight(_)) => MyLeft(List(a_err))
    case (MyRight(_), MyLeft(b_err)) => MyLeft(List(b_err))
    case (MyLeft(a_err), MyLeft(b_err)) => MyLeft(List(a_err, b_err))
  }

}


case class MyLeft[+E](value: E) extends MyEither[E, Nothing]


case class MyRight[+A](value: A) extends MyEither[Nothing, A]


object MyEither {


  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[List[E], List[A]] = {
    traverse(es)(x => x)
  }


  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[List[E], List[B]] = {
    as.foldRight[MyEither[List[E], List[B]]](MyRight(List[B]()))(
      (newElem, list) =>
        (f(newElem), list) match {
          case (MyRight(b_val), MyRight(b_list)) => MyRight(b_val :: b_list)
          case (MyRight(_), MyLeft(err_list)) => MyLeft(err_list)
          case (MyLeft(err_val), MyLeft(err_list)) => MyLeft(err_val :: err_list)
          case (MyLeft(err_val), MyRight(_)) => MyLeft(List(err_val))
        }
    )
  }

}
