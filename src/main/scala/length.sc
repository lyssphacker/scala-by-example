// exercise 9.2.1

def length(lst: List[Int]): Int = {
  def lengthIter(l: List[Int], result: Int): Int = {
    l match {
      case Nil => result
      case x :: xs => lengthIter(xs, result + 1)
    }
  }
  lengthIter(lst, 0)
}

