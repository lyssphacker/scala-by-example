abstract class Integer {
  def isZero: Boolean

  def predecessor: Integer

  def successor: Integer

  def +(that: Integer): Integer

  def -(that: Integer): Integer

  def isPositive: Boolean

  def negate: Integer
}

object Zero extends Integer {
  override def isZero: Boolean = true

  override def predecessor: Integer = new Pred(Zero)

  override def successor: Integer = new Succ(Zero)

  override def +(that: Integer): Integer = that

  override def -(that: Integer): Integer = if (that.isZero) Zero else new Pred(that.predecessor)

  override def isPositive = false

  override def negate = this

  override def toString = "0"
}

class Succ(x: Integer) extends Integer {
  override def isZero = false

  override def predecessor = x

  override def successor = new Succ(this)

  override def +(that: Integer) = x + that.successor

  override def -(that: Integer) = x - that.predecessor

  override def isPositive = true

  override def negate = Zero - x.successor

  override def toString = x.toString + "+1"
}

class Pred(x: Integer) extends Integer {
  override def isZero = false

  override def predecessor = new Pred(this)

  override def successor = x

  override def +(that: Integer) = x.predecessor + that

  override def -(that: Integer) = x.predecessor - that

  override def isPositive = false

  override def negate = Zero - x.predecessor

  override def toString = x.toString + "-1"
}

Zero - new Pred(Zero)
