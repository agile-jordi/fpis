package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  behavior of "pattern matching"

  it should "pattern match on liss" in {

    val l = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    assert(l === 3)

  }

  behavior of "List.tail"

  it should "fail on empty lists" in{
    intercept[NoSuchElementException]{
      List.tail(List())
    }
  }

  it should "return the tail of a non empty list" in {
    assert(List.tail(List(1,2,3)) === List(2,3))
  }

  behavior of "List.setHead"

  it should "fail on empty list" in {
    intercept[NoSuchElementException]{
      List.setHead(List(),23)
    }
  }

  it should "set the head of a non empty list" in {
    assert(List.setHead(List(1,2,3),23) === List(23,2,3))
  }

  behavior of "List.drop"

  it should "fail if we try to drop more elements than the list have" in{
    intercept[NoSuchElementException]{
      List.drop(List(1,2,3),4)
    }
  }

  it should "require n to be >= 0" in {
    intercept[IllegalArgumentException]{
      List.drop(List(1,2,3),-2)
    }
  }

  it should "drop no elements if n == 0" in{
    assert(List.drop(List(1,2,3),0) === List(1,2,3))
  }

  it should "drop first n elements" in {
    assert(List.drop(List(1,2,3,4),2) === List(3,4))
  }

  behavior of "List.dropWhile"

  it should "return the empty list for empty lists" in {
    assert(List.dropWhile(List())(_ => true) === List())
  }

  it should "drop the first elements while they match the predicate" in {
    assert(List.dropWhile(List(2,4,5,6))(_ % 2 == 0) === List(5,6))
  }

  behavior of "List.init"

  it should "fail on the empty list" in {
    intercept[NoSuchElementException]{
      List.init(List())
    }
  }

  it should "return all but the last element of a non empty list" in {
    assert(List.init(List(1,2,3,4)) === List(1,2,3))
  }

  behavior of "List.shortFoldRight (fold right with shortcut)"

  it should "shortcut" in {
    assert(List.shortFoldRight(List(1,0,2,3,4), 1, 0)(_ * _) === (0,1))
  }

  behavior of "List.foldRigth with list constructors"

  it should "construct the list" in{
    assert(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) === List(1,2,3))
  }

  behavior of "List.length"

  it should "return 0 for the empty list" in{
    assert(List.length(List()) === 0)
  }

  it should "return the length of a non empty list" in {
    assert(List.length(List(1,2,3)) === 3)
  }

}
