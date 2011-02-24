package passera

package object unsigned {
  // implicit def s2u(x: Int) = UInt(x)
  // implicit def u2s(x: UInt) = x.rep

  implicit def signedIntOps(x: Int) = new SignedIntOps(x)
  implicit def signedLongOps(x: Long) = new SignedLongOps(x)
  implicit def signedRichIntOps(x: scala.runtime.RichInt) = new SignedRichIntOps(x.self.asInstanceOf[Int])
  implicit def richUInt(x: UInt) = new RichUInt(x)
  implicit def richerUInt(x: UInt) = new RicherUInt(x.toInt)

  class SignedIntOps(x: Int) {
    def toUInt = UInt(x)

    def +(y: UInt) = x + y.rep
    def -(y: UInt) = x - y.rep
    def *(y: UInt) = x * y.rep
    def /(y: UInt) = x / y.rep
    def %(y: UInt) = x % y.rep
  }

  class SignedLongOps(x: Long) {
    def toUInt = UInt((x & 0xffffffffL).toInt)
  }

  class SignedRichIntOps(x: Int) {
    def to(y: UInt): Range.Inclusive = x to y.rep
    def until(y: UInt): Range = x until y.rep
    // def max(that: UInt) = if (x < that) that else x
    // def min(that: UInt) = if (x > that) that else x
  }

  trait UIntOrdering extends Ordering[UInt] {
    def compare(x: UInt, y: UInt) = 
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object UIntOrdering extends UIntOrdering

  trait UIntIsIntegral extends Integral[UInt] {
    def plus(x: UInt, y: UInt): UInt = x + y
    def minus(x: UInt, y: UInt): UInt = x - y
    def times(x: UInt, y: UInt): UInt = x * y
    def quot(x: UInt, y: UInt): UInt = x / y
    def rem(x: UInt, y: UInt): UInt = x % y
    def negate(x: UInt): UInt = -x
    def fromInt(x: Int): UInt = UInt(x)
    def toInt(x: UInt): Int = x.toInt
    def toLong(x: UInt): Long = x.toLong
    def toFloat(x: UInt): Float = x.toFloat
    def toDouble(x: UInt): Double = x.toDouble
  }
  implicit object UIntIsIntegral extends UIntIsIntegral with UIntOrdering

  class RicherUInt(rep: Int) {
    def bitCount = Integer.bitCount(rep)
    def highestOneBit = Integer.highestOneBit(rep)
    def lowestOneBit = Integer.lowestOneBit(rep)
    def numberOfLeadingZeros = Integer.numberOfLeadingZeros(rep)
    def numberOfTrailingZeros = Integer.numberOfTrailingZeros(rep)
    def reverse = UInt(Integer.reverse(rep))
    def reverseBytes = UInt(Integer.reverseBytes(rep))
    def rotateLeft(dist: Int) = UInt(Integer.rotateLeft(rep, dist))
    def rotateRight(dist: Int) = UInt(Integer.rotateRight(rep, dist))
    def signum = if (rep == 0) 0 else 1
  }

  class RichUInt(x: UInt) {
    def to(y: UInt): Range.Inclusive = x.rep to y.rep
    def until(y: UInt): Range = x.rep until y.rep

    def compare(y: UInt): Int = {
        if (x < y) 1
        else if (x > y) -1
        else 0
    }
    def max(y: UInt) = if (x < y) y else x
    def min(y: UInt) = if (x > y) y else x

    def abs = x
  }
}
