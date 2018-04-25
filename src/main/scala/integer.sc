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

  override def -(that: Integer): Integer = {
    def iter(i: Integer, count: Integer): Integer = {
      println("i: " + i + " count: " + count)
      if (count.isZero) i
      else if (that.isInstanceOf[Succ]) iter(new Pred(i), new Pred(count))
      else iter(new Succ(i), new Succ(count))
    }
    iter(Zero, that)
  }


  override def isPositive = false

  override def negate = this

  override def toString = "0"
}

class Succ(x: Integer) extends Integer {
  override def isZero = x.successor.isZero

  override def predecessor = x

  override def successor = new Succ(this)

  override def +(that: Integer) = x + that.successor

  override def -(that: Integer) = x - that.predecessor

  override def isPositive = true

  override def negate = Zero - x.successor

  override def toString = x.toString + "+1"
}

class Pred(x: Integer) extends Integer {
  override def isZero = x.predecessor.isZero

  override def predecessor = new Pred(this)

  override def successor = x

  override def +(that: Integer) = x.predecessor + that

  override def -(that: Integer) = x.predecessor - that

  override def isPositive = false

  override def negate = Zero - x.predecessor

  override def toString = x.toString + "-1"
}

new Succ(new Succ(Zero)).isZero

//Zero - new Succ(new Succ(Zero))

//Zero - new Pred(Zero)

//Zero - new Succ(Zero)

//new Succ(Zero).isZero

//new Succ(Zero).negate
//new Succ(new Succ(Zero)).negate
//new Succ(Zero).successor.negate

//Zero - new Succ(new Succ(Zero))
//Zero - new Succ(Zero)
//new Succ(Zero).successor

//new Succ(Zero).predecessor.isZero
//new Pred(Zero).successor.isZero
//new Succ(Zero).isZero
//new Succ(Zero).predecessor.isZero