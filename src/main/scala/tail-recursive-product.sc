def product(f: Int => Int)(a: Int, b: Int): Int = {
  def iter(a: Int, result: Int): Int = {
    if (a > b) result
    else iter(a + 1, result * f(a))
  }
  iter(a, 1)
}

product(x => x)(1, 10)