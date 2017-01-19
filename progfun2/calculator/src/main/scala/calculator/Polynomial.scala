package calculator
import math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    val bV = b()
    bV * bV - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val aV = a()
    val bV = b()
    val cV = c()
    val deltaV = delta()
    if (deltaV < 0) Set.empty
    else if (deltaV == 0) Set(-bV / 2 / aV)
    else Set(
      (-bV + sqrt(deltaV)) / 2 / aV,
      (-bV - sqrt(deltaV)) / 2 / aV
    )
  }
}
