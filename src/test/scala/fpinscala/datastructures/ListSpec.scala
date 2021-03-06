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

  it should "fail on empty lists" in {
    intercept[NoSuchElementException] {
      List.tail(List())
    }
  }

  it should "return the tail of a non empty list" in {
    assert(List.tail(List(1, 2, 3)) === List(2, 3))
  }

  behavior of "List.setHead"

  it should "fail on empty list" in {
    intercept[NoSuchElementException] {
      List.setHead(List(), 23)
    }
  }

  it should "set the head of a non empty list" in {
    assert(List.setHead(List(1, 2, 3), 23) === List(23, 2, 3))
  }

  behavior of "List.drop"

  it should "fail if we try to drop more elements than the list have" in {
    intercept[NoSuchElementException] {
      List.drop(List(1, 2, 3), 4)
    }
  }

  it should "require n to be >= 0" in {
    intercept[IllegalArgumentException] {
      List.drop(List(1, 2, 3), -2)
    }
  }

  it should "drop no elements if n == 0" in {
    assert(List.drop(List(1, 2, 3), 0) === List(1, 2, 3))
  }

  it should "drop first n elements" in {
    assert(List.drop(List(1, 2, 3, 4), 2) === List(3, 4))
  }

  behavior of "List.dropWhile"

  it should "return the empty list for empty lists" in {
    assert(List.dropWhile(List())(_ => true) === List())
  }

  it should "drop the first elements while they match the predicate" in {
    assert(List.dropWhile(List(2, 4, 5, 6))(_ % 2 == 0) === List(5, 6))
  }

  behavior of "List.init"

  it should "fail on the empty list" in {
    intercept[NoSuchElementException] {
      List.init(List())
    }
  }

  it should "return all but the last element of a non empty list" in {
    assert(List.init(List(1, 2, 3, 4)) === List(1, 2, 3))
  }

  behavior of "List.shortFoldRight (fold right with shortcut)"

  it should "shortcut" in {
    assert(List.shortFoldRight(List(1, 0, 2, 3, 4), 1, 0)(_ * _) ===(0, 1))
  }

  behavior of "List.foldRigth"

  it should "construct the list when passed in list constructors" in {
    assert(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) === List(1, 2, 3))
  }

  ignore should "stack overflow when given a really long list" in {
    // We can't use apply since it is not tail recursive neither
    val l = (1 to 10000).foldLeft(List[Int]())((acc, n) => Cons(n, acc))
    try {
      List.foldRight(l, 0)(_ + _)
      fail("StackOverflowError expected")
    } catch {
      case s: StackOverflowError => // Ok
    }
  }

  behavior of "List.length"

  it should "return 0 for the empty list" in {
    assert(List.length(List()) === 0)
  }

  it should "return the length of a non empty list" in {
    assert(List.length(List(1, 2, 3)) === 3)
  }

  behavior of "List.foldLeft"

  it should "foldLeft" in {
    assert(List.foldLeft(List(1, 2, 3), "0")(_ + _.toString) === "0123")
  }

  behavior of "List.sumFoldLeft"

  it should "sum a non empty list" in {
    assert(List.sumFoldLeft(List(1, 2, 3, 4)) === 10)
  }

  behavior of "List.productFoldLeft"

  it should "multiply a non empty list" in {
    assert(List.productFoldLeft(List(1, 2, 3, 4)) === 24)
  }

  behavior of "List.lengthFoldLeft"

  it should "calculate the length of a non empty list" in {
    assert(List.lengthFoldLeft(List(1, 2, 3, 4)) === 4)
  }

  behavior of "List.reverse"

  it should "reverse the empty list" in {
    assert(List.reverse(List()) === List())
  }

  it should "reverse the non empty list" in {
    assert(List.reverse(List(1, 2, 3, 4)) === List(4, 3, 2, 1))
  }

  behavior of "List.foldLeftInTermsOfFoldRight"

  it should "concatenate the elements of a non empty list" in {
    assert(List.foldLeftInTermsOfFoldRight(List(1, 2, 3), "0")(_ + _.toString) === "0123")
  }

  behavior of "List.foldRightInTermsOfFoldLeft"

  it should "concatenate the elements of a non empty list" in {
    assert(List.foldRightInTermsOfFoldLeft(List(1, 2, 3), "0")(_.toString + _) === "1230")
  }

  behavior of "appendViaFold"

  it should "append 2 lists" in {
    assert(List.appendViaFold(List(1, 2), List(3, 4, 5)) === List(1, 2, 3, 4, 5))
  }

  behavior of "listConcat"

  it should "concatenate a list of lists" in {
    assert(List.concat(List(List(1, 2), List(3, 4, 5), List(6))) === List(1, 2, 3, 4, 5, 6))
  }

  behavior of "map"

  it should "add 1 to each element" in {
    assert(List.map(List(3, 4, 5))(_ + 1) === List(4, 5, 6))
  }

  it should "convert to string" in {
    assert(List.map(List(1.1, 1.2, 1.3))(_.toString) === List("1.1", "1.2", "1.3"))
  }

  behavior of "filter"

  it should "filter elements from an empty list" in {
    assert(List.filter(List[Int]())(_ > 0) === List())
  }

  it should "filter elements from a non empty list" in {
    assert(List.filter(List(1, 2, 3, 4))(_ % 2 == 0) === List(2, 4))
  }

  behavior of "flatMap"

  it should "work on the empty list" in {
    assert(List.flatMap(List())(i => List(i, i)) === List())
  }

  it should "work on the non empty list" in {
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) === List(1, 1, 2, 2, 3, 3))
  }

  it should "work when f returns some emptuy lists" in {
    assert(List.flatMap(List(1, 2, 3))(i => if (i % 2 == 0) List() else List(i, i)) === List(1, 1, 3, 3))
  }

  behavior of "filterViaFlatMap"

  it should "filter elements from a non empty list" in {
    assert(List.filterViaFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) === List(2, 4))
  }

  behavior of "add"

  it should "add the elements in two empty lists" in {
    assert(List.add(List(), List()) === List())
  }

  it should "add two lists with the same length" in {
    assert(List.add(List(1, 2, 3), List(4, 5, 6)) === List(5, 7, 9))
  }

  it should "add two lists with different lenghts" in {
    assert(List.add(List(1, 2, 3), List(4, 5)) === List(5, 7))
  }

  behavior of "combine"

  it should "combine elements in two empty lists" in {
    assert(List.combine(List[String](), List[Int]())((s, i) => s + " -> " + i) === List())
  }

  it should "combine two lists with the same length" in {
    assert(List.combine(List("i", "ii"), List(1, 2))((s, i) => s + " -> " + i) === List("i -> 1", "ii -> 2"))
  }

  behavior of "startsWith"

  it should "calculate for the empty list" in {
    assert(List.startsWith(List(), List(1, 2)) === false)
  }

  it should "calculate for the empty prefix" in {
    assert(List.startsWith(List(1, 2), List()) === true)
  }

  it should "calculate for the empty list and prefix" in {
    assert(List.startsWith(List(), List()) === true)
  }

  it should "calculate for the non empty list and a non empty correct prefix" in {
    assert(List.startsWith(List(1, 2, 4, 5), List(1, 2)) === true)
  }

  it should "calculate for the non empty list and a false non empty prefix" in {
    assert(List.startsWith(List(1, 2, 3, 4), List(1, 5)) === false)
  }

  behavior of "hasSubsequence"

  it should "calculate for the empty list" in {
    assert(List.hasSubsequence(List(), List(1)) === false)
  }

  it should "calculate for the empty subsequence" in {
    assert(List.hasSubsequence(List(1, 2, 3), List()) === true)
  }

  it should "calculate for the empty list and subsequence" in {
    assert(List.hasSubsequence(List(), List()) === true)
  }

  it should "calculate for the non empty list and a non empty prefix" in {
    assert(List.hasSubsequence(List(1, 2, 4, 5), List(1, 2)) === true)
  }

  it should "calculate for the non empty list and non empty subsequence that is not a prefix" in {
    assert(List.hasSubsequence(List(1, 2, 4, 5), List(2, 4)) === true)
  }

  it should "calculate for the non empty list and a list which is not a subsequence" in {
    assert(List.hasSubsequence(List(1, 2, 4, 5), List(1, 4)) === false)
  }
}
