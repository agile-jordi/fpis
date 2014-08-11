package fpinscala.state

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  lazy val candiesLeft = candies > 0

  def insertCoin() = if (locked && candiesLeft) this.copy(locked = false, coins = coins + 1) else this

  def turnKnob() = if (!locked && candiesLeft) this.copy(locked = true, candies = candies - 1) else this
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

object Machine {

  private def applyInput(i: Input): State[Machine, Unit] = i match {
    case Coin => State.modify(_.insertCoin())
    case Turn => State.modify(_.turnKnob())
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(applyInput))
      s <- State.get
    } yield (s.candies, s.coins)

}