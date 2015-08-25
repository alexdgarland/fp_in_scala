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
    case x :: xs => xs  // This also covers x :: Nil, returning Nil (empty list if passed a list of only one element)
  }


  /*

  3.3 - SetHead function

  */

  def sethead[A](list : List[A], newHead : A) : List[A] = list match {
    case Nil => Nil
    case x :: xs => newHead :: xs
  }

}
