package fpinscala.errors

import Option.lift

object Math {

  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.size)

  def meanEither(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  val absO: Option[Double] => Option[Double] = lift(math.abs)
}
