// exercise 4.1.1
import scala.math.{abs, pow}

def sqrtIter(guess: Double, x: Double, tolerance: Double): Double =
  if (isGoodEnough(guess, x, tolerance)) guess
  else sqrtIter(improve(guess, x), x, tolerance)

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def isGoodEnough(guess: Double, x: Double, tolerance: Double) =
  abs(pow(guess, 2) - x) < tolerance

def sqrt(x: Double, tolerance: Double) = sqrtIter(1.0, x, tolerance)

// example invocations
sqrt(0.5, 0.000001)
sqrt(4000000000000000.0, 1)