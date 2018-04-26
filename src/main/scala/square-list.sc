// exercise 9.4.1

def squareList1(xs: List[Int]): List[Int] = xs match {
  case List() => Nil
  case y :: ys => (y * y) :: squareList1(ys)
}

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x * x)