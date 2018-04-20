// exercise 4.6.1

def factorial(n: Int) = {
  def factorialIter(n: Int, accumulator: Int): Int =
    if (n == 0) accumulator
    else factorialIter(n - 1, n * accumulator)
  factorialIter(n, 1)
}
