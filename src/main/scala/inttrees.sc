// exercise 7.2.2

abstract class IntTree

case object EmptyTree extends IntTree

case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree

def contains(t: IntTree, v: Int): Boolean = t match {
  case EmptyTree => false
  case Node(e, l, r) =>
    if (e == v) true
    else if (e < v) contains(r, v)
    else contains(l, v)
}

def insert(t: IntTree, v: Int): IntTree = t match {
  case EmptyTree => Node(v, EmptyTree, EmptyTree)
  case Node(e, l, r) =>
    if (e == v) t
    else if (e < v) Node(e, l, insert(r, v))
    else Node(e, insert(l, v), r)
}




