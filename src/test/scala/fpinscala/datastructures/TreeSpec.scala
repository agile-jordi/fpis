package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeSpec extends FlatSpec {

  behavior of "size"

  it should "calculate for a leaf" in {
    assert(Tree.size(Leaf(2)) === 1)
  }

  it should "calculate for a branch" in {
    assert(Tree.size(Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))) === 3)
  }

  behavior of "max"

  it should "calculate for a leaf" in {
    assert(Tree.max(Leaf(3)) === 3)
  }

  it should "calculate for a branch" in {
    assert(Tree.max(Branch(Leaf(3), Branch(Leaf(4), Leaf(3)))) === 4)
  }

  behavior of "depth"

  it should "calculate for a leaf" in {
    assert(Tree.depth(Leaf(2)) === 1)
  }

  it should "calculate for a branch" in {
    assert(Tree.depth(Branch(Leaf(3), Branch(Leaf(4), Leaf(3)))) === 3)
  }

  behavior of "map"

  it should "calculate for a leaf" in {
    assert(Tree.map(Leaf(3))(_ + 1) === Leaf(4))
  }

  it should "calculate for a branch" in {
    assert(Tree.map(Branch(Leaf(3), Branch(Leaf(4), Leaf(3))))(_ + 1) === Branch(Leaf(4), Branch(Leaf(5), Leaf(4))))
  }
}
