package passera

package object unsigned {
  // implicit def s2u(x: Int) = UInt(x)
  // implicit def u2s(x: UInt) = x.rep

  import Unsigned._

  implicit def signedIntOps(x: Int) = new SignedIntOps(x)
  implicit def signedLongOps(x: Long) = new SignedLongOps(x)
  implicit def signedRichIntOps(x: scala.runtime.RichInt) = new SignedRichIntOps(x.self.asInstanceOf[Int])
  implicit def richUInt(x: UInt) = new RichUInt(x)
  implicit def richerUInt(x: UInt) = new RicherUInt(x.toInt)

  type UInt = Unsigned.UInt
  val UInt = Unsigned.UInt

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
    def minus(x: UInt, y: UInt): UInt = UInt(x - y) // broken!
    def times(x: UInt, y: UInt): UInt = x * y
    def quot(x: UInt, y: UInt): UInt = x / y
    def rem(x: UInt, y: UInt): UInt = x % y
    def negate(x: UInt): UInt = UInt(-x)
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

  object Unsigned {
    import scala.math.{ScalaNumber, ScalaNumericConversions}

    @serializable
    case class UInt(override val intValue: Int) extends ScalaNumber with ScalaNumericConversions {
      private[unsigned] def rep = toInt

      // override def intValue = rep
      override def byteValue = rep.toByte
      override def shortValue = rep.toShort
      override def longValue = (rep & 0xffffffffL)
      override def floatValue = (rep & 0xffffffffL).toFloat
      override def doubleValue = (rep & 0xffffffffL).toDouble

      // Implementing ScalaNumber
      protected def isWhole: Boolean = true
      def underlying = this

      // Result of operation with a signed int is signed
      def +(x: Int) = rep + x
      def *(x: Int) = rep * x
      def -(x: Int) = rep - x
      def /(x: Int) = rep / x
      def %(x: Int) = rep % x

      def +(x: UInt) = UInt(rep + x.rep)
      def *(x: UInt) = UInt(rep * x.rep)
      def -(x: UInt) = rep - x.rep // signed, not unsigned!!! This breaks Numeric

      private def rot(x: Int) = (x + Int.MinValue)

      // Algorithm from Hacker's Delight
      def /(x: UInt) = {
        val n = rep & 0xffffffffL
        val m = x.rep & 0xffffffffL
        val r = n / m
        UInt(r.toInt)
        /*
        val n = rep
        val d = x.rep
        val t = d >> 31
        val n_ = n & ~t
        val q = ((n_ >>> 1) / d) << 1
        val r = n - q * d
        UInt(q + (if (rot(r) >= rot(d)) 1 else 0))
        */
      }

      // Algorithm from Hacker's Delight
      def %(x: UInt) = {
        val n = rep & 0xffffffffL
        val m = x.rep & 0xffffffffL
        val r = n % m
        UInt(r.toInt)
        /*
        val n = rep
        val d = x.rep
        val t = d >> 31
        val n_ = n & ~t
        val q = ((n_ >>> 1) / d) << 1
        UInt(n - q * d)
        */
      }

      override def toString = (rep & 0xffffffffL).toString + "u"

      def toHexString = (rep & 0xffffffffL).toHexString + "u"
      def toOctalString = (rep & 0xffffffffL).toOctalString + "u"
      def toBinaryString = (rep & 0xffffffffL).toBinaryString + "u"

      // Equality comparison to UInt is baked in

      // Override equals to allow comparison with other number types.
      // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
      // comparing a number on the left with a UInt on the right.
      // This is an (undocumented?) hack and might change in the future.
      override def equals(x: Any) = x match {
        case x: UInt => this.toInt == x.toInt
        case x: Int => this.toInt == x && x >= 0
        case x: Number => this.longValue == x.longValue
        case _ => false
      }
      override def canEqual(x: Any) = x match {
        case _: UInt => true
        case _: Number => true
        case _ => false
      }

      // Here, compare to Int
      // def ==(x: Int) = rep == x && rep >= 0
      // def !=(x: Int) = rep != x || rep < 0

      def <(x: UInt) = rot(rep) < rot(x.rep)
      def >(x: UInt) = rot(rep) > rot(x.rep)
      def <=(x: UInt) = rot(rep) <= rot(x.rep)
      def >=(x: UInt) = rot(rep) >= rot(x.rep)

      def +(x : java.lang.String) = this.toString + x

      def &(x : UInt) = UInt(rep & x.rep)
      def |(x : UInt) = UInt(rep | x.rep)
      def ^(x : UInt) = UInt(rep ^ x.rep)

      def <<(x : Int) = UInt(rep << x)
      def <<(x : Long) = UInt(rep << x)
      def >>(x : Long) = UInt(rep >>> x)
      def >>(x : Int) = UInt(rep >>> x)
      def >>>(x : Int) = UInt(rep >>> x)
      def >>>(x : Long) = UInt(rep >>> x)
      def <<(x : UInt) = UInt(rep >>> (x.rep & 0x1f))
      def >>(x : UInt) = UInt(rep << (x.rep & 0x1f))
      def >>>(x : UInt) = UInt(rep >>> (x.rep & 0x1f))

      def unary_+ = this
      def unary_- = -rep   // signed!  Could be confusing.
      def unary_~ = UInt(~rep)
    }
  }
}
