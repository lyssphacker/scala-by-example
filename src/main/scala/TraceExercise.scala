import org.slf4s.LoggerFactory

import scala.math.{abs, pow}

class TraceExercise {
  val log = LoggerFactory.getLogger[TraceExercise]

  def sqrtIter(guess: Double, x: Double): Double = {
    log.info(s"guess: $guess, x: $x");
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  def isGoodEnough(guess: Double, x: Double) =
    abs(pow(guess, 2) - x) < 0.001

  def sqrt(x: Double) = sqrtIter(1.0, x)
}
