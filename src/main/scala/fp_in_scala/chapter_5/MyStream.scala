package fp_in_scala.chapter_5


sealed trait MyStream[+A] {


  import MyStream._


  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, t) => Some(h())
  }


  def toList: List[A] = this match {
    case MyEmpty => List.empty
    case MyCons(h, t) => h() :: t().toList
  }


  def take(n: Int): MyStream[A] = this match {
    case MyCons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }


  @annotation.tailrec
  final def drop(n: Int): MyStream[A] = this match {
    case MyCons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }


  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }


  def exists(p: A => Boolean): Boolean = this match {
    case MyCons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case MyCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }


  def forAll(p: A => Boolean): Boolean = this match {
    case MyCons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }


  def takeWhileUsingFoldRight(p: A => Boolean): MyStream[A] = {
    this.foldRight(empty[A])(
      (nextElement, stream) =>
        if (p(nextElement)) cons(nextElement, stream)
        else empty
    )
  }


  def headOptionUsingFoldRight: Option[A] =
    this.foldRight[Option[A]](None)(
      (nextElement, stream) => Some(nextElement)
    )


  def map[B](f: A => B): MyStream[B] = this match {
    case MyCons(h, t) => cons[B](f(h()), t().map(f))
    case _ => empty
  }


  def filter(p: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t) if p(h()) => cons[A](h(), t().filter(p))
    case MyCons(h, t) => t().filter(p)
    case _ => empty
  }


  def append[B >: A](other: MyStream[B]): MyStream[B] =
    foldRight(other)(cons(_,_))

}


case object MyEmpty extends MyStream[Nothing]


case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]


object MyStream {


  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }


  def empty[A]: MyStream[A] = MyEmpty


  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
