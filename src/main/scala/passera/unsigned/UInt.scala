package passera

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
  def -(x: UInt) = UInt(rep - x.rep)

  private def rot(x: Int) = (x + Int.MinValue)

  def /(x: UInt) = {
    val n = rep & 0xffffffffL
    val m = x.rep & 0xffffffffL
    val r = n / m
    UInt(r.toInt)
    /*
    // Algorithm from Hacker's Delight
    val n = rep
    val d = x.rep
    val t = d >> 31
    val n_ = n & ~t
    val q = ((n_ >>> 1) / d) << 1
    val r = n - q * d
    UInt(q + (if (rot(r) >= rot(d)) 1 else 0))
    */
  }

  def %(x: UInt) = {
    val n = rep & 0xffffffffL
    val m = x.rep & 0xffffffffL
    val r = n % m
    UInt(r.toInt)
    /*
    // Algorithm from Hacker's Delight
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
  def unary_- = UInt(-rep)
  def unary_~ = UInt(~rep)
}
