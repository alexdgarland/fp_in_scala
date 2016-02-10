package fp_in_scala.chapter_4

/**
  *
  * Functions implemented to complete exercises and generally explore concepts from
  * Chiusano, Bjarnason - "Functional Programming in Scala"
  *
  */

case class MySome[+A](get: A) extends MyOption[A]


case class MyNone() extends MyOption[Nothing]


sealed trait MyOption[+A] {


  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone() => MyNone()
    case MySome(a) => MySome(f(a))
  }


  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone() => MyNone()
    case MySome(a) => f(a)
  }


  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone() => default
    case MySome(a) => a
  }


  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MySome(a) => this
    case MyNone() => ob
  }


  def filter(f: A => Boolean): MyOption[A] = {
    if (map(f).getOrElse(false)) this else MyNone()
  }


}
