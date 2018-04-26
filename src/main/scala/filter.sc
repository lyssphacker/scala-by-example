// exercise 9.4.2

def filter(p: Int => Boolean, lst: List[Int]): List[Int] = {
  lst match {
    case Nil => lst
    case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
  }
}

def forall(p: Int => Boolean, lst: List[Int]): Boolean = {
  filter(p, lst).length == lst.length
}

def exists(p: Int => Boolean, lst: List[Int]): Boolean = {
  filter(p, lst).nonEmpty
}
