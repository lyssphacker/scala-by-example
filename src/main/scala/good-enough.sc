// exercise 4.1.1
import scala.math.{abs, pow}

def sqrtIter(guess: Double, x: Double, precision: Double): Double =
  if (isGoodEnough(guess, x, precision)) guess
  else sqrtIter(improve(guess, x), x, precision)

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def isGoodEnough(guess: Double, x: Double, precision: Double) =
  abs(pow(guess, 2) - x) < precision

def sqrt(x: Double, precision: Double) = sqrtIter(1.0, x, precision)

// example invocations
sqrt(0.5, 0.000001)
sqrt(4000000000000000.0, 1)