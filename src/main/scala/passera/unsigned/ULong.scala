package passera.unsigned

import Ops.{UnsignedOpWorkaround, SignedOpWorkaround}

class ULong(override val longValue: Long) extends AnyVal with Unsigned[ULong, ULong, Long] {
  private[unsigned] def rep = longValue

  def toUByte = UByte((rep & 0xffffffffL).toByte)
  def toUShort = UShort((rep & 0xffffffffL).toShort)
  def toUInt = UInt((rep & 0xffffffffL).toInt)
  def toULong = this

  def byteValue = rep.toByte
  def shortValue = rep.toShort
  def intValue = rep.toInt
  // def longValue = rep
  def floatValue = rep.toFloat
  def doubleValue = rep.toDouble

  def +(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong + x
  def -(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong - x
  def *(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong * x
  def /(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong / x
  def %(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong % x
  def &(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong & x
  def ^(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong ^ x
  def |(x: Int)(implicit ignore: SignedOpWorkaround): Long = this.toLong | x

  def +(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong + x
  def -(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong - x
  def *(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong * x
  def /(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong / x
  def %(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong % x
  def &(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong & x
  def ^(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong ^ x
  def |(x: Long)(implicit ignore: SignedOpWorkaround): Long = this.toLong | x

  def +(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this + x.toULong
  def -(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this - x.toULong
  def *(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this * x.toULong
  def /(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this / x.toULong
  def %(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this % x.toULong
  def &(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this & x.toULong
  def ^(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this ^ x.toULong
  def |(x: UByte)(implicit ignore: UnsignedOpWorkaround): ULong = this | x.toULong
  def <(x: UByte): Boolean = this < x.toULong
  def >(x: UByte): Boolean = this > x.toULong
  def <=(x: UByte): Boolean = this <= x.toULong
  def >=(x: UByte): Boolean = this >= x.toULong

  def +(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this + x.toULong
  def -(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this - x.toULong
  def *(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this * x.toULong
  def /(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this / x.toULong
  def %(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this % x.toULong
  def &(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this & x.toULong
  def ^(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this ^ x.toULong
  def |(x: UShort)(implicit ignore: UnsignedOpWorkaround): ULong = this | x.toULong
  def <(x: UShort): Boolean = this < x.toULong
  def >(x: UShort): Boolean = this > x.toULong
  def <=(x: UShort): Boolean = this <= x.toULong
  def >=(x: UShort): Boolean = this >= x.toULong

  def +(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this + x.toULong
  def -(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this - x.toULong
  def *(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this * x.toULong
  def /(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this / x.toULong
  def %(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this % x.toULong
  def &(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this & x.toULong
  def ^(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this ^ x.toULong
  def |(x: UInt)(implicit ignore: UnsignedOpWorkaround): ULong = this | x.toULong
  def <(x: UInt): Boolean = this < x.toULong
  def >(x: UInt): Boolean = this > x.toULong
  def <=(x: UInt): Boolean = this <= x.toULong
  def >=(x: UInt): Boolean = this >= x.toULong

  def +(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = ULong(rep + x.rep)
  def -(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = ULong(rep - x.rep)
  def *(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = ULong(rep * x.rep)

  def rot(x: Long) = (x + Long.MinValue)

  def /(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = {
    val n = rep
    val d = x.rep

    if (true) return {
      if (d < 0) {
        if (this < x)
          ULong(0l)
        else
          ULong(1l)
      }
      else {
        val q = ((n >>> 1) / d) << 1
        val r = n - q * d
        if (ULong(r) >= x)
          ULong(q+1)
        else
          ULong(q)
      }
    }

    val t = d >> 63
    val n1 = n & ~t
    val a = n1 >>> 1
    val b = a / d
    val q0 = b << 1
    val r = n - q0 * d
    val q = q0 + (if (ULong(r) >= x) 1l else 0l)
    ULong(q.toLong)
  }

  def %(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong = {
    val n = rep
    val d = x.rep


    val t = d >> 63
    val n1 = n & ~t
    val a = n1 >>> 1
    val b = a / d
    val q0 = b << 1
    val r = n - q0 * d
    // val q = q0 + (if (ULong(r) >= ULong(d)) 1 else 0)
    ULong(r.toLong)
  }

  override def toString =
    if (rep >= 0L)
      rep.toString
    else if (rep == 1 << 63)
      (rep.toString).tail
    else
      (~(rep - 1)).toString

  def toHexString = rep.toHexString
  def toOctalString = rep.toOctalString
  def toBinaryString = rep.toBinaryString

  // Here, compare to Int
  // def ==(x: Int) = rep == x && rep >= 0
  // def !=(x: Int) = rep != x || rep < 0

  def <(x: ULong) = rot(rep) < rot(x.rep)
  def >(x: ULong) = rot(rep) > rot(x.rep)
  def <=(x: ULong) = rot(rep) <= rot(x.rep)
  def >=(x: ULong) = rot(rep) >= rot(x.rep)

  def +(x : java.lang.String) = this.toString + x

  def &(x : ULong)(implicit ignore: UnsignedOpWorkaround) = ULong(rep & x.rep)
  def |(x : ULong)(implicit ignore: UnsignedOpWorkaround) = ULong(rep | x.rep)
  def ^(x : ULong)(implicit ignore: UnsignedOpWorkaround) = ULong(rep ^ x.rep)

  def <<(x : Int)(implicit ignore: SignedOpWorkaround) = ULong(rep << x)
  def <<(x : Long)(implicit ignore: SignedOpWorkaround) = ULong(rep << x)
  def >>(x : Long)(implicit ignore: SignedOpWorkaround) = ULong(rep >>> x)
  def >>(x : Int)(implicit ignore: SignedOpWorkaround) = ULong(rep >>> x)
  def >>>(x : Int)(implicit ignore: SignedOpWorkaround) = ULong(rep >>> x)
  def >>>(x : Long)(implicit ignore: SignedOpWorkaround) = ULong(rep >>> x)

  def <<(x : UInt)(implicit ignore: UnsignedOpWorkaround) = ULong(rep >>> (x.rep & 0x3f))
  def <<(x : ULong)(implicit ignore: UnsignedOpWorkaround) = ULong(rep >>> (x.rep & 0x3f))
  def >>(x : UInt)(implicit ignore: UnsignedOpWorkaround) = ULong(rep << (x.rep & 0x3f))
  def >>(x : ULong)(implicit ignore: UnsignedOpWorkaround) = ULong(rep << (x.rep & 0x3f))
  def >>>(x : UInt)(implicit ignore: UnsignedOpWorkaround) = ULong(rep >>> (x.rep & 0x3f))
  def >>>(x : ULong)(implicit ignore: UnsignedOpWorkaround) = ULong(rep >>> (x.rep & 0x3f))

  def unary_+ = this
  def unary_- = ULong(-rep)
  def unary_~ = ULong(~rep)
}

object ULong {
  def MinValue = ULong(0L)
  def MaxValue = ULong(~0L)

  def apply(v: Long) = new ULong(v)
}
