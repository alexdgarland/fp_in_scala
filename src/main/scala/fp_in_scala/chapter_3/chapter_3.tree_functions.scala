package fp_in_scala.chapter_3

/**
 *
 * Functions implemented to complete exercises and generally explore concepts from
 * Chiusano, Bjarnason - "Functional Programming in Scala"
 *
 */

object tree_functions {

  /*

  Tree implmentation lifted directly from the book:

  */
  sealed trait Tree[+A]

  case class Leaf[A](value : A) extends Tree[A]

  case class Branch[A](left : Tree[A], right : Tree[A]) extends Tree[A]


  /*

  Functions operating on tree:

  */

  import scala.math.max

  /*

  3.25 "size" - count the number of nodes (leaves and branches) in a generic Tree.

  */

  def treeSize[A](tree : Tree[A]) : Int = tree match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + treeSize(left) + treeSize(right)
  }


  /*

  3.26 "maximum" - return the maximum element (leaf value) in a Tree[Int].

  */

  def treeMaxValue(tree : Tree[Int]) : Int = tree match {
    case Leaf(value) => value
    case Branch(left,right) => max(treeMaxValue(left), treeMaxValue(right))
  }


  /*

  3.27 "depth" - get the maximum path length from root to leaf in a generic Tree.

  */

  def treeDepth[A](tree : Tree[A]) : Int = tree match {
    case Leaf(_) => 0
    case Branch(left,right) => 1 + max(treeDepth(left), treeDepth(right))
  }

  /*

  3.28 "map" - modify each element in a tree with a given function.

  I'm assuming that:
    a) this is supposed to be a pure function, hence returning a new immutable tree
    b) mapped transformations are limited to operating on leaf values (not branch structure)

  */

  def treeMap[A,B](tree : Tree[A])(f : A=>B) : Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left,right) => Branch(treeMap(left)(f), treeMap(right)(f))
  }


  /*

  3.29 Generalise tree functions by implementing "fold".

  */

  def treeFold[A,B](tree : Tree[A])(fLeaf : A=>B)(fBranch : (B,B) => B) : B = tree match {
    case Leaf(value) => fLeaf(value)
    case Branch(left,right) => fBranch(treeFold(left)(fLeaf)(fBranch),treeFold(right)(fLeaf)(fBranch))
  }

  def treeSizeWithFold[A](tree : Tree[A]) =
    treeFold(tree)(leaf=>1)((lSize,rSize)=> 1 + lSize + rSize)

  def treeMaxValueWithFold(tree : Tree[Int]) =
    treeFold(tree)(v=>v)((lMax,rMax)=>max(lMax,rMax))

  def treeDepthWithFold[A](tree : Tree[A]) =
    treeFold(tree)(_=>0)((lDepth,rDepth)=> 1 + max(lDepth,rDepth))

  def treeMapWithFold[A,B](tree : Tree[A])(f : A=>B) =
    treeFold(tree)(v=>Leaf(f(v)):Tree[B])((lTree, rTree)=>Branch(lTree,rTree))

}
