// exercise 5.2.2

def product(f: Int => Int)(a: Int, b: Int): Int = {
  def iter(a: Int, result: Int): Int = {
    if (a > b) result
    else iter(a + 1, result * f(a))
  }
  iter(a, 1)
}

// exercise 5.2.3

def factorial(n: Int) = product(x => x)(1, n)

factorial(10)