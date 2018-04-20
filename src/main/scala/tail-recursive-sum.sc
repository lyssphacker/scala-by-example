def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def iter(a: Int, result: Int): Int = {
    if (a > b) result
    else iter(a + 1, result + a)
  }
  iter(a, 0)
}

sum(x => x)(1, 10)