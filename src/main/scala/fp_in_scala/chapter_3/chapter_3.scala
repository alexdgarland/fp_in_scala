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
      case x::xs if p(x) => dropwhile(xs, p)
      case _ => list
  }


  /*

  3.6 Init function

  Because we're working with an immutable, singly-linked list, we have to:

  a) loop through fully
    (as there is no direct pointer to the last element)

  b) construct each time without simply data-sharing
    (as the link from penultimate to last element cannot be simply removed/ updated)

  */

  def init[A](list : List[A]) : List[A] = list match {
    case Nil => Nil
    case x::Nil => Nil
    case x::xs => x::init(xs)
  }


  /*

  3.9 Length using foldRight

  */

  def lengthWithFoldRight[A](list : List[A]) : Int = {
    list.foldRight(0)((_,acc)=>acc+1)
  }


  /*

  3.10 foldLeft implementations

  */

  // Iterative version roughly in line with Odersky implementation for core Scala
  def iterFoldLeft[A,B](list : List[A], startVal : B)(f: (B,A) => B) : B = {
    var acc : B = startVal
    var remainingList : List[A] = list
    while(!(remainingList.isEmpty)) {
      acc = f(acc, remainingList.head)
      remainingList = remainingList.tail
    }
    acc
  }

  // Tail-recursive equivalent
  @annotation.tailrec
  def recFoldLeft[A,B](list : List[A], startVal : B)(f: (B,A) => B) : B = list match {
    case Nil => startVal
    case x::xs => recFoldLeft(xs, f(startVal,x))(f)
  }


  /*

  3.11 Functions implemented with (built-in) foldLeft

  */

  def sumWithFoldLeft(list : List[Int]) = {
    list.foldLeft(0)(_+_)
  }

  def productWithFoldLeft(list : List[Int]) = {
    list.foldLeft(1)(_*_)
  }

  def lengthWithFoldLeft[A](list : List[A]) = {
    list.foldLeft(0)((acc,_)=>acc+1)
  }


  /*

  3.12 Reverse list - implemented with foldLeft

  */

  def reverseWithFoldLeft[A](list : List[A]) : List[A] = {
    list.foldLeft(List.empty[A])((acc,next)=>next::acc)
  }

}
