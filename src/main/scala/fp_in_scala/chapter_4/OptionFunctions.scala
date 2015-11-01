package fp_in_scala.chapter_4

object OptionFunctions {

  // Exercise 4.4 - implement "map2",
  // applying  a binary function if both Options given are Some
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=>C) : Option[C] =
    (a,b) match {
      case (Some(val_a), Some(val_b)) => Some(f(val_a, val_b))
      case  _ => None
    }

}
