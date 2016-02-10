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
