package fpinscala.errors

object Math {

  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
  }

  def mean(xs:Seq[Double]):Option[Double] = if(xs.isEmpty) None else Some(xs.sum / xs.size)

}
