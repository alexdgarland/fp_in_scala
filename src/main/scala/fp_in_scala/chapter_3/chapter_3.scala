package fp_in_scala.chapter_3

/**
 *
 * Functions implemented to complete exercises and generally explore concepts from
 * Chiusano, Bjarnason - "Functional Programming in Scala"
 *
 */

object functions {

  /*

  3.2 - Tail function

  */

  def tail[A](list : List[A]) : List[A] = list match {
    case Nil => Nil
    case x :: xs => xs
    // This also covers x :: Nil (returning empty list if passed list of one element)
  }


  /*

  3.3 - SetHead function

  */

  def sethead[A](list : List[A], newHead : A) : List[A] = list match {
    case Nil => Nil
    case x :: xs => newHead :: xs
  }


  /*

  3.4 - Drop function

  */

  @annotation.tailrec
  def drop[A](list : List[A], n : Int) : List[A] = {
    if (list == Nil) Nil
    else if (n <= 0) list
    else drop(tail(list), n-1)
  }


  /*

  3.5 Drop While function

  */

  @annotation.tailrec
  def dropwhile[A](list : List[A], p : (A) => Boolean) : List[A] = list match {
      case Nil => Nil
      case x::xs if(p(x)) => dropwhile(xs, p)
      case x::xs => x::xs
  }

}
