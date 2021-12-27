package calculator

object Polynomial extends PolynomialInterface :

  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal( b() * b() - 4 * a() * c() )

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val d = math.sqrt(delta())

      if (delta() < 0) Set.empty // delta negative, there is a 0 root.
      else if (delta() > 0) Set( // delta positive, there are 2 root.
        (-b() + d) / (2 * a()),
        (-b() - d) / (2 * a()) )
      else Set( // delta is 0, so there are 1 root.
        b() / 2 * a() )
    }
