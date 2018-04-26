// exercise 9.1.1

def isort(xs: List[Int]): List[Int] =
  if (xs.isEmpty) Nil
  else insert(xs.head, isort(xs.tail))

def insert(x: Int, xs: List[Int]): List[Int] = {
  if (xs.isEmpty) x :: xs
  else if (x < xs.head) x :: insert(xs.head, xs.tail)
  else xs.head :: insert(x, xs.tail)
}

