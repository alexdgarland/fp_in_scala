package fp_in_scala.chapter_4

object OptionFunctions {


  // Exercise 4.3 - implement "map2",
  // applying a binary function if both Options given are Some
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  // We can do this using pattern matching ...
  //    (a,b) match {
  //      case (Some(val_a), Some(val_b)) => Some(f(val_a, val_b))
  //      case  _ => None
  //    }
  // ... but this is a more idiomatic version as provided later in the chapter:
    a.flatMap(a_value => b.map(b_value => f(a_value, b_value)))


  // Exercise 4.4 - implement "sequence",
  // turning a List[Option] into an Option[List]
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    /*
    This works but has a worst-case runtime of 2n in the case where there are no Nones
    (goes over the whole list to check for None, then over it again to extract the values from each Some).
    There's an obvious way to do this O(n) iteratively (& must be an equivalent recursive/ declarative way as well).
     */
    // if (a.contains(None)) None else Some(a.map(elem=>elem.get))

    /*
    An implementation using foldRight and map2, again essentially cribbed from:
    https://github.com/astorije/fpinscala-exercises/blob/master/src/main/scala/ch4errorhandling/Option.scala#L84
     */
    // a.foldRight[Option[List[A]]](Some(List()))((oElem,oList)=>map2(oElem,oList)(_::_))
    /*
    map2 allows None values to be propagated through foldRight,
    allowing screening for None and getting values to be done in a single pass
    while also blocking further applications of the mapped function as soon as any None is found.
     */

    // Following Ex. 4.5 - Rewrite in terms of traverse
    // Effectively just pass the identity function - traverse is a generalisation of sequence
    traverse(a)(elem => elem)
  }


  // Exercise 4.5 - implement "traverse"
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(List[B]()))((oElem, oList) => map2(f(oElem), oList)(_ :: _))
  }

}
