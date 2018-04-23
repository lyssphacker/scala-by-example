trait IntSet {
  def elem: Int
  def left: IntSet
  def right: IntSet
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(s: IntSet): IntSet
  def intersection(s: IntSet): IntSet
  def isLeaf: Boolean = left.isInstanceOf[EmptySet] && right.isInstanceOf[EmptySet]
}

class EmptySet extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)

  def union(s: IntSet) = s

  def intersection(s: IntSet) = new EmptySet

  def elem = ???

  def left = ???

  def right = ???

  override def toString: String = ""
}

class NonEmptySet(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this

  def union(s: IntSet): IntSet = {
    def unionIter(s: IntSet, result: IntSet): IntSet = {
      if (s.isInstanceOf[EmptySet]) result
      else {
        val leftResult = unionIter(s.left, result incl s.elem)
        unionIter(s.right, leftResult)
      }
    }
    unionIter(s, this)
  }

  override def intersection(s: IntSet): IntSet = ???

  override def toString: String = s"$elem ${left.toString} ${right.toString}"
}

val s1 = new NonEmptySet(1, new EmptySet, new EmptySet)
val s2 = new NonEmptySet(2, new EmptySet, new EmptySet)
val s21 = s2 incl 3 incl 4 incl 5
val s11 = s1 incl 6 incl 7 incl 1

val s3 = s11 union s21


