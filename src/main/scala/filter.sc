def filter(p: Int => Boolean, lst: List[Int]): List[Int] = {
  lst match {
    case Nil => lst
    case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
  }
}

