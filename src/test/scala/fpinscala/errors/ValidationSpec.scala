package fpinscala.errors

import org.scalatest.FlatSpec
import fpinscala.datastructures.List
import scala.util.control.NonFatal

class ValidationSpec extends FlatSpec {

  private val twoErrors: Validation[String, String] = Error[String](List("Number not even", "Number too small"))

  behavior of "map"

  it should "map over a ok value" in {
    assert(Ok("World").map(s => s"Hello $s") === Ok("Hello World"))
  }

  it should "map over an error value" in {
    assert(twoErrors.map(_ + 1) === twoErrors)
  }

  behavior of "flatMap"

  private val parseInt: (String) => Validation[String, Int] = s => try {
    Ok(s.toInt)
  } catch {
    case NonFatal(_) => Error(List(s"$s is not an int"))
  }

  it should "flatMap over an error value" in {
    assert(twoErrors.flatMap(parseInt) === twoErrors)
  }

  it should "flatMap over an ok value with a ok function" in {
    assert(Ok("3").flatMap(parseInt) === Ok(3))
  }

  it should "flatMap over an ok value with a error function" in {
    assert(Ok("a").flatMap(parseInt) === Error(List("a is not an int")))
  }

  behavior of "orElse"

  it should "orElse an error or an ok value" in {
    assert(twoErrors.orElse(Ok(3)) === Ok(3))
  }

  it should "orElse an error or another error" in {
    assert(twoErrors.orElse(Error(List("Ugly number"))) === Error(List("Number not even", "Number too small", "Ugly number")))
  }

  it should "orElse an ok value or something else" in {
    assert(Ok(3).orElse(Error(List("Wrong!"))) === Ok(3))
  }

  behavior of "map2"

  it should "map2 over ok and ok" in {
    assert(parseInt("3").map2(parseInt("5"))(_ + _) === Ok(8))
  }

  it should "map2 over ok and error" in {
    assert(parseInt("3").map2(parseInt("b"))(_ + _) === Error(List("b is not an int")))
  }

  it should "map2 over error and ok" in {
    assert(parseInt("a").map2(parseInt("5"))(_ + _) === Error(List("a is not an int")))
  }

  it should "map2 over error and error" in {
    assert(parseInt("a").map2(parseInt("b"))(_ + _) === Error(List("a is not an int", "b is not an int")))
  }

}
