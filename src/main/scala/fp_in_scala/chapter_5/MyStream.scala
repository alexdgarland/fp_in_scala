package fp_in_scala.chapter_5


sealed trait MyStream[+A] {


  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, t) => Some(h())
  }


  def toList: List[A] = this match {
    case MyEmpty => List.empty
    case MyCons(h, t) => h() :: t().toList
  }


  def take(n: Int): MyStream[A] = this match {
    case MyCons(h, t) if n > 0 => MyStream.cons(h(), t().take(n - 1))
    case _ => MyStream.empty
  }


  @annotation.tailrec
  final def drop(n: Int): MyStream[A] = this match {
    case MyCons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }


  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t) if p(h()) => MyStream.cons(h(), t().takeWhile(p))
    case _ => MyStream.empty
  }

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


  def apply[A](as: A*): MyStream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
