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


  /*

  3.13 Implementing foldLeft and foldRight in terms of each other.

  This is a "hard" problem so not solving for now.

  For reference, there looks to be a good explanation of the answer on Stack Overflow here:

  http://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala

  */


  /*

  3.14 Implement append using foldLeft &/or foldRight

  In this case, foldRight seems the obvious way to do it.

  */

  def appendWithFoldRight[A](list : List[A], newElement : A) : List[A] = {
    list.foldRight(List(newElement))((elem, accList) => elem :: accList)
  }


  /*

  3.15 Concatenate a list-of-lists into a single list in linear time (AKA flatten).

  */

  def flattenWithFoldRight[A](nestedList : List[List[A]]) : List[A] = {

    // This is effectively an inner loop running once for each element of the individual list passed.
    def merge(outList : List[A], nextList : List[A]) = {
      nextList.foldRight(outList)((nextElem, list) => nextElem :: list)
    }

    // This is effectively an outer loop, running the merge function (inner loop) once for each list in the input.
    nestedList.foldRight(List.empty[A])((nextList, outList) => merge(outList, nextList))

    // So, each element of each list is run over once, giving us the required linear (O(n)) time
    // (relative to the total length of the output list).

  }


  /*

  3.16 Transform a list of integers by adding 1 to each (pure - returning new list).

  This is obviously aiming at "map" so will do our own quick implementation of that first.

  */

  // Note: had to write this curried to allow proper type inference when used.
  def mapWithFoldRight[A,B](inList : List[A])(f : (A) => (B)) : List[B] = {
    inList.foldRight(List.empty[B])((nextElem, list) => f(nextElem) :: list)
  }

  def incrementList(list : List[Int]) = {
    mapWithFoldRight(list)((element)=>(element + 1))
  }


  /*

  3.17 Turn each value in a List[Double] into a string.

  This is another use for map.

  */

  def stringifyDoubleList(list : List[Double]) : List[String] = {
    mapWithFoldRight(list)(d=>d.toString)
  }


  /*

  3.18 Function "map" that generalises modifying each list element while maintaining structure.

  This is already implemented as part of 3.16!

  */


  /*

  3.19 Implement filter and use it to remove odd numbers from a list.

  */

  def filterWithFoldRight[A] (list : List[A]) (predicate : (A)=>Boolean) : List[A] = {
    list.foldRight(List.empty[A])((elem, list) => if (predicate(elem)) elem::list else list)
  }

  def removeOddNumbers(list : List[Int]) : List[Int] = {
    filterWithFoldRight(list)(i=>(i%2==0))
  }


  /*

  3.20 Implement flatMap

  */

  def myFlatMap[A,B](list : List[A])(f : A => List[B]) = {

    // This is probably best (cerainly most simply) implemented by composing or chaining other functions,
    // for example "list.map(f).flatten" (this passes unit test).

    // However, just to explore what's going on (based on my implementation of flatten):

    def merge(outList : List[B], nextList : List[B]) = {
      nextList.foldRight(outList)((nextElem, list) => nextElem :: list)
    }

    list.foldRight(List.empty[B])((elem, outList) => merge(outList, f(elem)))

  }


  /*

  3.21 Use flatMap to implement filter

  */

  def filterWithFlatMap[A](list : List[A])(predicate : A=>Boolean) = {
    list.flatMap(elem=> if (predicate(elem)) List(elem) else List.empty[A])
  }


  /*

  3.22 Construct a list from two lists of ints by adding corresponding elements

  and

  3.23 Generalise the function (implement zipWith)

  I'm approaching this by simply implementing zipWith and then using it to handle the more specific version.

  */

  def myZipWith[A,B,C](list1 : List[A], list2 : List[B])(f : (A,B)=>C) : List[C] = {

    val sharedLength = scala.math.min(list1.length, list2.length)

    var outList = List.empty[C]

    for (i <- ((sharedLength-1) to 0 by -1)) {
      outList = f(list1(i), list2(i)) :: outList
    }

    outList
  }

  def zipWithAddition(list1 : List[Int], list2 : List[Int]) = myZipWith(list1, list2)(_+_)

}
