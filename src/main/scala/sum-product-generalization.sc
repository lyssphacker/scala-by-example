def foo(f: Int => Int)(g: (Int, Int) => Int)(a: Int, b: Int)(init: Int): Int = {
  def iter(a: Int, result: Int): Int = {
    if (a > b) result
    else iter(a + 1, g(result, f(a)))
  }
  iter(a, init)
}

val sum1To10 = foo(x => x)((x, y) => x + y)(1, 10)(0)
val product1To10 = foo(x => x)((x, y) => x * y)(1, 10)(1)
