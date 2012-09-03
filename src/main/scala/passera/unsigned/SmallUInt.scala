package passera.unsigned

import scala.math.{ScalaNumber, ScalaNumericConversions}

/**
 * Supertrait of UByte, UShort, UInt
 */
@serializable
trait SmallUInt[U <: Unsigned[U, UInt, Int]] extends Unsigned[U, UInt, Int] {
  private def intRep = intValue

  def toUByte = UByte(intRep.toByte)
  def toUShort = UShort(intRep.toShort)
  def toUInt = UInt(intRep)
  def toULong = ULong(intRep & 0xffffffffL)

  override def byteValue = intRep.toByte
  override def shortValue = intRep.toShort
  override def intValue: Int
  override def longValue = (intRep & 0xffffffffL)
  override def floatValue = (intRep & 0xffffffffL).toFloat
  override def doubleValue = (intRep & 0xffffffffL).toDouble

  def +(x: Int): Int = this.toInt + x
  def -(x: Int): Int = this.toInt - x
  def *(x: Int): Int = this.toInt * x
  def /(x: Int): Int = this.toInt / x
  def %(x: Int): Int = this.toInt % x
  def &(x: Int): Int = this.toInt & x
  def |(x: Int): Int = this.toInt | x
  def ^(x: Int): Int = this.toInt ^ x

  def +(x: Long): Long = this.toLong + x
  def -(x: Long): Long = this.toLong - x
  def *(x: Long): Long = this.toLong * x
  def /(x: Long): Long = this.toLong / x
  def %(x: Long): Long = this.toLong % x
  def &(x: Long): Long = this.toLong & x
  def |(x: Long): Long = this.toLong | x
  def ^(x: Long): Long = this.toLong ^ x

  def +(x: UByte): UInt = this + x.toUInt
  def -(x: UByte): UInt = this - x.toUInt
  def *(x: UByte): UInt = this * x.toUInt
  def /(x: UByte): UInt = this / x.toUInt
  def %(x: UByte): UInt = this % x.toUInt
  def &(x: UByte): UInt = this & x.toUInt
  def |(x: UByte): UInt = this | x.toUInt
  def ^(x: UByte): UInt = this ^ x.toUInt
  def <(x: UByte): Boolean = this < x.toUInt
  def >(x: UByte): Boolean = this > x.toUInt
  def <=(x: UByte): Boolean = this <= x.toUInt
  def >=(x: UByte): Boolean = this >= x.toUInt

  def +(x: UShort): UInt = this + x.toUInt
  def -(x: UShort): UInt = this - x.toUInt
  def *(x: UShort): UInt = this * x.toUInt
  def /(x: UShort): UInt = this / x.toUInt
  def %(x: UShort): UInt = this % x.toUInt
  def &(x: UShort): UInt = this & x.toUInt
  def |(x: UShort): UInt = this | x.toUInt
  def ^(x: UShort): UInt = this ^ x.toUInt
  def <(x: UShort): Boolean = this < x.toUInt
  def >(x: UShort): Boolean = this > x.toUInt
  def <=(x: UShort): Boolean = this <= x.toUInt
  def >=(x: UShort): Boolean = this >= x.toUInt

  def +(x: ULong): ULong = this.toULong + x
  def -(x: ULong): ULong = this.toULong - x
  def *(x: ULong): ULong = this.toULong * x
  def /(x: ULong): ULong = this.toULong / x
  def %(x: ULong): ULong = this.toULong % x
  def &(x: ULong): ULong = this.toULong & x
  def |(x: ULong): ULong = this.toULong | x
  def ^(x: ULong): ULong = this.toULong ^ x
  def <(x: ULong): Boolean = this.toULong < x
  def >(x: ULong): Boolean = this.toULong > x
  def <=(x: ULong): Boolean = this.toULong <= x
  def >=(x: ULong): Boolean = this.toULong >= x

  def +(x: UInt) = UInt(intRep + x.intRep)
  def -(x: UInt) = UInt(intRep - x.intRep)
  def *(x: UInt) = UInt(intRep * x.intRep)

  def /(x: UInt) = {
    val n = intRep & 0xffffffffL
    val m = x.intRep & 0xffffffffL
    val r = n / m
    UInt(r.toInt)
  }

  def %(x: UInt) = {
    val n = intRep & 0xffffffffL
    val m = x.intRep & 0xffffffffL
    val r = n % m
    UInt(r.toInt)
  }

  def unary_+ = this.toUInt
  def unary_- = UInt(-intRep)

  // Equality comparison to UInt is baked in

  // Override equals to allow comparison with other number types.
  // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
  // comparing a number on the left with a UInt on the right.
  // This is an (undocumented?) hack and might change in the future.
  override def equals(x: Any) = x match {
    case x: SmallUInt[_] => this.toInt == x.intRep
    case x: ULong => this.toULong == x
    case x: Number => this.longValue == x.longValue && x.longValue >= 0
    case _ => false
  }
  def canEqual(x: Any) = x match {
    case _: SmallUInt[_] => true
    case _: ULong => true
    case _: Number => true
    case _ => false
  }

  private def rot(x: Int) = (x + Int.MinValue)

  def <(x: UInt) = rot(intRep) < rot(x.intRep)
  def >(x: UInt) = rot(intRep) > rot(x.intRep)
  def <=(x: UInt) = rot(intRep) <= rot(x.intRep)
  def >=(x: UInt) = rot(intRep) >= rot(x.intRep)

  def &(x : UInt) = UInt(intRep & x.intRep)
  def |(x : UInt) = UInt(intRep | x.intRep)
  def ^(x : UInt) = UInt(intRep ^ x.intRep)

  def unary_~ = UInt(~intRep)

  def <<(x : Int) = UInt(intRep << x)
  def <<(x : Long) = UInt(intRep << x)
  def <<(x : UInt) = UInt(intRep << (x.toInt & 0x1f))
  def <<(x : ULong) = UInt(intRep << (x.toLong & 0x1f))

  def >>(x : Long) = UInt(intRep >>> x)
  def >>(x : Int) = UInt(intRep >>> x)
  def >>(x : UInt) = UInt(intRep >>> (x.toInt & 0x1f))
  def >>(x : ULong) = UInt(intRep >>> (x.toLong & 0x1f))

  def >>>(x : Int) = UInt(intRep >>> x)
  def >>>(x : Long) = UInt(intRep >>> x)
  def >>>(x : UInt) = UInt(intRep >>> (x.toInt & 0x1f))
  def >>>(x : ULong) = UInt(intRep >>> (x.toLong & 0x1f))

  override def toString = (intRep & 0xffffffffL).toString

  def +(x : java.lang.String) = this.toString + x

  def toHexString = (intRep & 0xffffffffL).toHexString
  def toOctalString = (intRep & 0xffffffffL).toOctalString
  def toBinaryString = (intRep & 0xffffffffL).toBinaryString
}
