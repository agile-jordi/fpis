package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {

  behavior of "size"

  it should "calculate for a leaf" in{
    assert(Tree.size(Leaf(2)) === 1)
  }

  it should "calculate for a branch" in {
    assert(Tree.size(Branch(Leaf(2),Branch(Leaf(3),Leaf(4)))) === 3)
  }
}
