// exercise 10.3.1

def flatten[A](xss: List[List[A]]): List[A] =
  for(xs <- xss; x <- xs) yield x