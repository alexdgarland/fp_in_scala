package fp_in_scala.chapter_3

import fp_in_scala.chapter_3.tree_functions._
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner._


@RunWith(classOf[JUnitRunner])
class Chapter3TreeTests extends Specification {


  /*

            Tree used for testing:

               root(branch)
              /           \
             /             \
       branch             branch
       /   \             /      \
      /     \           /        \
 leaf(1)  leaf(3)    leaf(2)    branch
                               /      \
                              /        \
                           leaf(5)     leaf(4)

  Hence:  size = 9
          max value = 5
          max depth (path length) = 3

  */

  val testTree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(2), Branch(Leaf(5), Leaf(4))))

  "function for size of tree" should {

    "return 9 for standard test tree" in {
      treeSize(testTree) must beEqualTo(9)
    }

    "with fold - return 9 for standard test tree" in {
      treeSizeWithFold(testTree) must beEqualTo(9)
    }

  }

  "function for maximum leaf value in tree" should {

    "return 5 for standard test tree" in {
      treeMaxValue(testTree) must beEqualTo(5)
    }

    "with fold - return 5 for standard test tree" in {
      treeMaxValueWithFold(testTree) must beEqualTo(5)
    }

  }

  "function for depth of tree" should {

    "return 3 for standard test tree" in {
      treeDepth(testTree) must beEqualTo(3)
    }

    "with fold - return 3 for standard test tree" in {
      treeDepthWithFold(testTree) must beEqualTo(3)
    }

  }

  "function for mapping over tree" should {

    val expectedIntTree = Branch(Branch(Leaf(2), Leaf(6)), Branch(Leaf(4), Branch(Leaf(10), Leaf(8))))
    val expectedStringTree = Branch(Branch(Leaf("A2"), Leaf("A6")), Branch(Leaf("A4"), Branch(Leaf("A10"), Leaf("A8"))))

    "return expected tree for standard test tree with doubling function mapped" in {
      treeMap(testTree)(i => i * 2) must beEqualTo(expectedIntTree)
    }

    "return expected tree for standard test tree with function which changes value type mapped" in {
      treeMap(testTree)(i => "A" + (i * 2).toString) must beEqualTo(expectedStringTree)
    }

    "with fold - return expected tree for standard test tree with doubling function mapped" in {
      treeMapWithFold(testTree)(i => i * 2) must beEqualTo(expectedIntTree)
    }

    "with fold - return expected tree for standard test tree with function which changes value type mapped" in {
      treeMapWithFold(testTree)(i => "A" + (i * 2).toString) must beEqualTo(expectedStringTree)
    }


  }


}
