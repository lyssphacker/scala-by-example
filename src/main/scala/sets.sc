trait IntSet {
  def elem: Int
  def left: IntSet
  def right: IntSet
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(s: IntSet): IntSet
  def intersection(s: IntSet): IntSet
}

class EmptySet extends IntSet {
  override def contains(x: Int): Boolean = false

  override def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)

  override def union(s: IntSet) = s

  override def intersection(s: IntSet) = new EmptySet

  override def elem = ???

  override def left = ???

  override def right = ???
}

class NonEmptySet(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this

  override def union(s: IntSet): IntSet = {
    def unionIter(s: IntSet, result: IntSet): IntSet = {
      if (s.isInstanceOf[EmptySet]) result
      else {
        val leftResult = unionIter(s.left, result incl s.elem)
        unionIter(s.right, leftResult)
      }
    }
    unionIter(s, this)
  }

  override def intersection(s: IntSet): IntSet = {
    def intersectionIter(s: IntSet, result: IntSet): IntSet = {
      if (s.isInstanceOf[EmptySet]) result
      else {
        val leftResult =
          if (this.contains(s.elem)) intersectionIter(s.left, result incl s.elem)
          else intersectionIter(s.left, result)
        intersectionIter(s.right, leftResult)
      }
    }
    intersectionIter(s, new EmptySet)
  }
}



