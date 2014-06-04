package fpinscala.errors

import org.scalatest.FlatSpec
import fpinscala.datastructures.List
import scala.util.control.NonFatal

class EitherSpec extends FlatSpec{

  behavior of "map"

  val wrong:Either[String,String] = Left("Wrong!")

  it should "map over a left" in {
    assert(wrong.map(s => s"Hello $s") === wrong)
  }

  it should "map over a right" in {
    assert(Right("World").map(s => s"Hello $s") === Right("Hello World"))
  }

  behavior of "flatMap"

  it should "flatMap over a left" in{
    assert(wrong.flatMap(s => Right(s"Hello $s")) === wrong)
  }

  it should "flatMap over a right resulting in left" in {
    assert(Right("World").flatMap(s => Left("No!")) === Left("No!"))
  }

  it should "flatMap over a right resulting in right" in {
    assert(Right("World").flatMap(s => Right(s"Hello $s")) === Right("Hello World"))
  }

  behavior of "orElse"

  it should "orElse over a left resulting in right" in {
    assert(wrong.orElse(Right("Hey!")) === Right("Hey!"))
  }

  it should "orElse over a left resulting in left" in {
    assert(wrong.orElse(Left("Nope!")) === Left("Nope!"))
  }

  it should "orElse over a right" in {
    assert(Right("Hi!").orElse(Left("No!")) === Right("Hi!"))
  }

  behavior of "map2"

  it should "map2 over two rights" in{
    assert(Right("Hello").map2(Right(" World"))(_ + _) === Right("Hello World"))
  }

  it should "map2 over a left and a right" in{
    assert(wrong.map2(Right(" World"))(_ + _) === wrong)
  }

  it should "map2 over a right and a left" in{
    assert(Right("Hello").map2(wrong)(_ + _) === wrong)
  }

  it should "map2 over two lefts" in {
    val no:Either[String,String] = Left("No!")
    assert(wrong.map2(no)(_ + _) === wrong)
  }

  behavior of "traverse"

  import Either.traverse

  private val parseInt:(String => Either[String,Int]) = (s:String) => try{
    Right(s.toInt)
  } catch {
    case NonFatal(_) => Left(s"$s is not an int")
  }

  it should "traverse an empty list" in{
    assert(traverse(List.apply[String]())(parseInt) === Right(List.apply[String]()))
  }

  it should "traverse a non empty list with no errors" in{
    assert(traverse(List("1","2","3"))(parseInt) === Right(List(1,2,3)))
  }

  it should "traverse a non empty list and return the first error" in {
    assert(traverse(List("1","2","a","b"))(parseInt) === Left("a is not an int"))
  }

  behavior of "sequence"

  import Either.sequence

  it should "sequence an empty list" in{
    assert(sequence(List.apply[Either[String,Int]]()) === Right(List.apply[Either[String,Int]]()))
  }

  it should "sequence a non empty list with no errors" in{
    assert(sequence(List(Right(1),Right(2),Right(3))) === Right(List(1,2,3)))
  }

  it should "sequence a non empty list and return the first error" in {
    assert(sequence(List(Right(1),Right(2),Left("a is not an int"),Left("b is not an int"))) === Left("a is not an int"))
  }



}
