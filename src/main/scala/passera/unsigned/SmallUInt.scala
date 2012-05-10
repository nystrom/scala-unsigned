package passera.unsigned

import Ops.{UnsignedOpWorkaround, SignedOpWorkaround}

/**
 * Supertrait of UByte, UShort, UInt
 */
trait SmallUInt[U <: Unsigned[U, UInt, Int]] extends Any with Unsigned[U, UInt, Int] {
  private[unsigned] def intRep = intValue

  def toUByte = UByte(intRep.toByte)
  def toUShort = UShort(intRep.toShort)
  def toUInt = UInt(intRep)
  def toULong = ULong(intRep & 0xffffffffL)

  def byteValue = intRep.toByte
  def shortValue = intRep.toShort
  // def intValue: Int = intRep
  def longValue = (intRep & 0xffffffffL)
  def floatValue = (intRep & 0xffffffffL).toFloat
  def doubleValue = (intRep & 0xffffffffL).toDouble

  def +(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt + x
  def -(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt - x
  def *(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt * x
  def /(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt / x
  def %(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt % x
  def &(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt & x
  def |(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt | x
  def ^(x: Int)(implicit ignore: SignedOpWorkaround): Int = this.toInt ^ x

  def +(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong + x
  def -(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong - x
  def *(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong * x
  def /(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong / x
  def %(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong % x
  def &(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong & x
  def |(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong | x
  def ^(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong ^ x

  def +(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this + x.toUInt
  def -(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this - x.toUInt
  def *(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this * x.toUInt
  def /(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this / x.toUInt
  def %(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this % x.toUInt
  def &(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this & x.toUInt
  def |(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this | x.toUInt
  def ^(x: UByte)(implicit ignore: UnsignedOpWorkaround): UInt = this ^ x.toUInt
  def <(x: UByte): Boolean = this < x.toUInt
  def >(x: UByte): Boolean = this > x.toUInt
  def <=(x: UByte): Boolean = this <= x.toUInt
  def >=(x: UByte): Boolean = this >= x.toUInt

  def +(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this + x.toUInt
  def -(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this - x.toUInt
  def *(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this * x.toUInt
  def /(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this / x.toUInt
  def %(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this % x.toUInt
  def &(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this & x.toUInt
  def |(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this | x.toUInt
  def ^(x: UShort)(implicit ignore: UnsignedOpWorkaround): UInt = this ^ x.toUInt
  def <(x: UShort): Boolean = this < x.toUInt
  def >(x: UShort): Boolean = this > x.toUInt
  def <=(x: UShort): Boolean = this <= x.toUInt
  def >=(x: UShort): Boolean = this >= x.toUInt

  def +(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong + x
  def -(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong - x
  def *(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong * x
  def /(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong / x
  def %(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong % x
  def &(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong & x
  def |(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong | x
  def ^(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = this.toULong ^ x
  def <(x: ULong): Boolean = this.toULong < x
  def >(x: ULong): Boolean = this.toULong > x
  def <=(x: ULong): Boolean = this.toULong <= x
  def >=(x: ULong): Boolean = this.toULong >= x

  def +(x: UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep + x.intRep)
  def -(x: UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep - x.intRep)
  def *(x: UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep * x.intRep)

  def /(x: UInt)(implicit ignore: UnsignedOpWorkaround) = {
    val n = intRep & 0xffffffffL
    val m = x.intRep & 0xffffffffL
    val r = n / m
    UInt(r.toInt)
  }

  def %(x: UInt)(implicit ignore: UnsignedOpWorkaround) = {
    val n = intRep & 0xffffffffL
    val m = x.intRep & 0xffffffffL
    val r = n % m
    UInt(r.toInt)
  }

  def unary_+ = this.toUInt
  def unary_- = UInt(-intRep)

  private def rot(x: Int) = (x + Int.MinValue)

  def <(x: UInt) = rot(intRep) < rot(x.intRep)
  def >(x: UInt) = rot(intRep) > rot(x.intRep)
  def <=(x: UInt) = rot(intRep) <= rot(x.intRep)
  def >=(x: UInt) = rot(intRep) >= rot(x.intRep)

  def &(x : UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep & x.intRep)
  def |(x : UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep | x.intRep)
  def ^(x : UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep ^ x.intRep)

  def unary_~ = UInt(~intRep)

  def <<(x : Int)(implicit ignore: SignedOpWorkaround) = UInt(intRep << x)
  def <<(x : Long)(implicit ignore: SignedOpWorkaround) = UInt(intRep << x)
  def >>(x : Long)(implicit ignore: SignedOpWorkaround) = UInt(intRep >>> x)
  def >>(x : Int)(implicit ignore: SignedOpWorkaround) = UInt(intRep >>> x)
  def >>>(x : Int)(implicit ignore: SignedOpWorkaround) = UInt(intRep >>> x)
  def >>>(x : Long)(implicit ignore: SignedOpWorkaround) = UInt(intRep >>> x)

  def <<(x : UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep >>> (x.toInt & 0x1f))
  def <<(x : ULong)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep >>> (x.toLong & 0x1f))
  def >>(x : UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep << (x.toInt & 0x1f))
  def >>(x : ULong)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep << (x.toLong & 0x1f))
  def >>>(x : UInt)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep >>> (x.toInt & 0x1f))
  def >>>(x : ULong)(implicit ignore: UnsignedOpWorkaround) = UInt(intRep >>> (x.toLong & 0x1f))

  override def toString = (intRep & 0xffffffffL).toString

  def +(x : java.lang.String) = this.toString + x

  def toHexString = (intRep & 0xffffffffL).toHexString
  def toOctalString = (intRep & 0xffffffffL).toOctalString
  def toBinaryString = (intRep & 0xffffffffL).toBinaryString
}
