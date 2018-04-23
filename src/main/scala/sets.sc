// exercises 6.0.1 and 6.0.2
trait IntSet {
  def elem: Int
  def left: IntSet
  def right: IntSet
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(s: IntSet): IntSet
  def intersection(s: IntSet): IntSet
  def isEmpty: Boolean
  def excl(x: Int): IntSet
}

class EmptySet extends IntSet {
  override def contains(x: Int): Boolean = false

  override def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)

  override def union(s: IntSet) = s

  override def intersection(s: IntSet) = new EmptySet

  override def elem = ???

  override def left = ???

  override def right = ???

  override def isEmpty = true

  override def toString: String = ""

  override def excl(x: Int) = this
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
      if (s.isEmpty) result
      else {
        val leftResult = unionIter(s.left, result incl s.elem)
        unionIter(s.right, leftResult)
      }
    }
    unionIter(s, this)
  }

  override def intersection(s: IntSet): IntSet = {
    def intersectionIter(s: IntSet, result: IntSet): IntSet = {
      if (s.isEmpty) result
      else {
        val leftResult =
          if (this.contains(s.elem)) intersectionIter(s.left, result incl s.elem)
          else intersectionIter(s.left, result)
        intersectionIter(s.right, leftResult)
      }
    }
    intersectionIter(s, new EmptySet)
  }

  override def isEmpty = false

  override def toString: String = s"$elem ${left.toString} ${right.toString}"

  override def excl(x: Int) = {
    def exclIter(s: IntSet, result: IntSet): IntSet = {
      if (s.isEmpty) result
      else {
        val leftResult =
          if (s.elem != x) exclIter(s.left, result incl s.elem)
          else exclIter(s.left, result)
        exclIter(s.right, leftResult)
      }
    }
    exclIter(this, new EmptySet)
  }
}

val s1 = new NonEmptySet(1, new EmptySet, new EmptySet)
val s2 = new NonEmptySet(2, new EmptySet, new EmptySet)
val s21 = s2 incl 3 incl 4 incl 5 incl 1
val s11 = s1 incl 6 incl 7 incl 1
val s3 = s11 intersection  s21

s21.excl(1)
s11.excl(6)


