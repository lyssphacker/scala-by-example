// exercise 9.4.3

def mapFun[A, B](xs: List[A], f: A => B): List[B] =
  (xs :\ List[B]()){ (x, xs) => f(x) :: xs }

def lengthFun[A](xs: List[A]): Int =
  (0 /: xs){ (z, x) => z + 1 }