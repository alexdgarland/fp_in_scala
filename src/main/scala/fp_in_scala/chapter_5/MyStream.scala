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


  def isEmpty: Boolean = this match {
    case MyEmpty => true
    case _ => false
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


  def map[B](f: A => B): MyStream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))


  def filter(p: A => Boolean): MyStream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)


  def append[B >: A](other: => MyStream[B]): MyStream[B] =
    foldRight(other)(cons(_, _))


  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(empty[B])(
      (newStream, existingStream) =>
        f(newStream).append(existingStream)
    )


  def mapUsingUnfold[B](f: A => B): MyStream[B] = {
    unfold(this)((str: MyStream[A]) =>
      str match {
        case MyCons(h, t) =>
          Some((f(h()), t()))
        case _ =>
          None
      })
  }


  def takeUsingUnfold(n: Int): MyStream[A] = {
    unfold((this, n)) {
      case (str: MyCons[A], i: Int) if i >= 1 =>
        Some((str.h(), (str.t(), i - 1)))
      case _ =>
        None
    }
  }


  def takeWhileUsingUnfold(p: A => Boolean): MyStream[A] = {
    unfold(this) {
      case (MyCons(h, t)) if p(h()) =>
        Some((h(), t()))
      case _ =>
        None
    }
  }


  def zipWithUsingUnfold[B, C](other: MyStream[B], f: (A, B) => C): MyStream[C] = {
    unfold((this, other)) {
      case (MyCons(h1, t1), MyCons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ =>
        None
    }
  }


  def zipAllUsingUnfold[B](other: MyStream[B]): MyStream[(Option[A], Option[B])] = {
    unfold((this, other)) {
      case (MyCons(h1, t1), MyCons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (MyCons(h1, t1), empty) =>
        Some((Some(h1()), None), (t1(), empty))
      case (empty, MyCons(h2, t2)) =>
        Some((None, Some(h2())), (empty, t2()))
      case _ =>
        None
    }
  }


  def startsWithUsingUnfold[B >: A](other: MyStream[B]): Boolean = {
    unfold(this, other) {
      case (MyCons(h1, t1), MyCons(h2, t2)) =>
        Some(h1() == h2(), (t1(), t2()))
      case (empty, MyCons(_, _)) =>
        Some(false, (empty, empty))
      case (_, empty) =>
        None
    }
      .forAll(b => b)
  }


  def tailsUsingUnfold: MyStream[MyStream[A]] = {

    case class AppendEmpty(b: Boolean)

    unfold(this, AppendEmpty(false)) {
      case (MyCons(h, t), _) =>
        Some(MyCons(h, t), (t(), AppendEmpty(true)))
      case (MyEmpty, AppendEmpty(true)) =>
        Some(MyStream.empty, (MyStream.empty, AppendEmpty(false)))
      case _ =>
        None
    }

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


  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => MyStream.empty
    }
  }


  def constant[A](a: A): MyStream[A] = {
    // cons(a, constant(a))
    unfold(a)(_ => Some(a, a))
  }


  def from(n: Int): MyStream[Int] = {
    // cons(n, from(n + 1))
    unfold(n)(i => Some(i, i + 1))
  }


  def fibs: MyStream[Int] = {

    //    def inner(a: Int, b: Int): Stream[Int] = cons(a, inner(b, a + b))
    //
    //    inner(0, 1)

    unfold((0, 1)) {
      case (a, b) => Some((a, (b, a + b)))
    }

  }

}
