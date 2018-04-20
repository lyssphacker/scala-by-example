// exercise 5.3.1

import scala.math.{abs, pow}

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def crt(x: Double) = fixedPoint(averageDamp(y => x/pow(y, 2)))(1.0)