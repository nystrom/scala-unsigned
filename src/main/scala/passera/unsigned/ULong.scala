package passera.unsigned

import scala.math.{ScalaNumber, ScalaNumericConversions}

@serializable
case class ULong(override val longValue: Long) extends AnyVal with Unsigned[ULong, ULong, Long] {
  private[unsigned] def rep = longValue

  def toUByte = UByte((rep & 0xffffffffL).toByte)
  def toUShort = UShort((rep & 0xffffffffL).toShort)
  def toUInt = UInt((rep & 0xffffffffL).toInt)
  def toULong = this

  override def toByte: Byte = (rep & 0x7fffffffffffffffL).toByte
  override def toChar: Char = (rep & 0x7fffffffffffffffL).toChar
  override def toShort: Short = (rep & 0x7fffffffffffffffL).toShort
  override def toInt: Int = (rep & 0x7fffffffffffffffL).toInt
  override def toLong: Long = rep
  override def toFloat: Float = (rep & 0x7fffffffffffffffL).toFloat
  override def toDouble: Double = (rep >>> 1).toDouble * 2. + (rep & 1L)

  // override def intValue = rep
  override def byteValue = toByte
  override def shortValue = toShort
  override def intValue = toInt
  override def floatValue = toFloat
  override def doubleValue = toDouble

  def +(x: Int)(implicit d: DummyImplicit): Long = this.toLong + x
  def -(x: Int)(implicit d: DummyImplicit): Long = this.toLong - x
  def *(x: Int)(implicit d: DummyImplicit): Long = this.toLong * x
  def /(x: Int)(implicit d: DummyImplicit): Long = this.toLong / x
  def %(x: Int)(implicit d: DummyImplicit): Long = this.toLong % x
  def &(x: Int)(implicit d: DummyImplicit): Long = this.toLong & x
  def ^(x: Int)(implicit d: DummyImplicit): Long = this.toLong ^ x
  def |(x: Int)(implicit d: DummyImplicit): Long = this.toLong | x

  def +(x: Long)(implicit d: DummyImplicit): Long = this.toLong + x
  def -(x: Long)(implicit d: DummyImplicit): Long = this.toLong - x
  def *(x: Long)(implicit d: DummyImplicit): Long = this.toLong * x
  def /(x: Long)(implicit d: DummyImplicit): Long = this.toLong / x
  def %(x: Long)(implicit d: DummyImplicit): Long = this.toLong % x
  def &(x: Long)(implicit d: DummyImplicit): Long = this.toLong & x
  def ^(x: Long)(implicit d: DummyImplicit): Long = this.toLong ^ x
  def |(x: Long)(implicit d: DummyImplicit): Long = this.toLong | x

  def +(x: UByte): ULong = this + x.toULong
  def -(x: UByte): ULong = this - x.toULong
  def *(x: UByte): ULong = this * x.toULong
  def /(x: UByte): ULong = this / x.toULong
  def %(x: UByte): ULong = this % x.toULong
  def &(x: UByte): ULong = this & x.toULong
  def ^(x: UByte): ULong = this ^ x.toULong
  def |(x: UByte): ULong = this | x.toULong
  def <(x: UByte): Boolean = this < x.toULong
  def >(x: UByte): Boolean = this > x.toULong
  def <=(x: UByte): Boolean = this <= x.toULong
  def >=(x: UByte): Boolean = this >= x.toULong

  def +(x: UShort): ULong = this + x.toULong
  def -(x: UShort): ULong = this - x.toULong
  def *(x: UShort): ULong = this * x.toULong
  def /(x: UShort): ULong = this / x.toULong
  def %(x: UShort): ULong = this % x.toULong
  def &(x: UShort): ULong = this & x.toULong
  def ^(x: UShort): ULong = this ^ x.toULong
  def |(x: UShort): ULong = this | x.toULong
  def <(x: UShort): Boolean = this < x.toULong
  def >(x: UShort): Boolean = this > x.toULong
  def <=(x: UShort): Boolean = this <= x.toULong
  def >=(x: UShort): Boolean = this >= x.toULong

  def +(x: UInt): ULong = this + x.toULong
  def -(x: UInt): ULong = this - x.toULong
  def *(x: UInt): ULong = this * x.toULong
  def /(x: UInt): ULong = this / x.toULong
  def %(x: UInt): ULong = this % x.toULong
  def &(x: UInt): ULong = this & x.toULong
  def ^(x: UInt): ULong = this ^ x.toULong
  def |(x: UInt): ULong = this | x.toULong
  def <(x: UInt): Boolean = this < x.toULong
  def >(x: UInt): Boolean = this > x.toULong
  def <=(x: UInt): Boolean = this <= x.toULong
  def >=(x: UInt): Boolean = this >= x.toULong

  def +(x: ULong): ULong = ULong(rep + x.rep)
  def -(x: ULong): ULong = ULong(rep - x.rep)
  def *(x: ULong): ULong = ULong(rep * x.rep)

  private def rot(x: Long) = (x + Long.MinValue)

  def /(x: ULong): ULong = {
    val n = rep
    val d = x.rep

    if (d < 0) {
      if (this < x)
        ULong(0l)
      else
        ULong(1l)
    } else {
      val q = ((n >>> 1) / d) << 1
      val r = n - q * d
      if (ULong(r) >= x)
        ULong(q+1)
      else
        ULong(q)
    }
  }

  def %(x: ULong): ULong = {
    val n = rep
    val d = x.rep

    if (d < 0) {
      if (this < x)
        ULong(n)
      else
        ULong(n - d)
    } else {
      val q = ((n >>> 1) / d) << 1
      val r = n - q * d
      if (ULong(r) >= x)
        ULong(r - d)
      else
        ULong(r)
    }
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

  // Equality comparison to ULong is baked in

  def ==(x: Int)(implicit d: DummyImplicit) = intValue == x
  def ==(x: Long)(implicit d: DummyImplicit) = longValue == x
  def ==(x: UInt) = longValue == x.longValue
  // def ==(x: ULong) = longValue == x.longValue
  def ==(x: Float) = floatValue == x
  def ==(x: Double) = doubleValue == x

  def !=(x: Int)(implicit d: DummyImplicit) = intValue != x
  def !=(x: Long)(implicit d: DummyImplicit) = longValue != x
  def !=(x: UInt) = longValue != x.longValue
  // def !=(x: ULong) = longValue != x.longValue
  def !=(x: Float) = floatValue != x
  def !=(x: Double) = doubleValue != x

  /*
  // Override equals to allow comparison with other number types.
  // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
  // comparing a number on the left with a UInt on the right.
  // This is an (undocumented?) hack and might change in the future.
  override def equals(x: Any) = x match {
    case x: UInt => this == x.toULong
    case x: ULong => this.toLong == x.toLong
    case x: Int => this.toInt == x && x >= 0
    case x: Long => this.toLong == x && x >= 0
    case x: Number => this.longValue == x.longValue
    case _ => false
  }
  override def canEqual(x: Any) = x match {
    case _: UInt => true
    case _: ULong => true
    case _: Number => true
    case _ => false
  }
  */

  // Here, compare to Int
  // def ==(x: Int) = rep == x && rep >= 0
  // def !=(x: Int) = rep != x || rep < 0

  def <(x: ULong) = rot(rep) < rot(x.rep)
  def >(x: ULong) = rot(rep) > rot(x.rep)
  def <=(x: ULong) = rot(rep) <= rot(x.rep)
  def >=(x: ULong) = rot(rep) >= rot(x.rep)

  def +(x : java.lang.String) = this.toString + x

  def &(x : ULong) = ULong(rep & x.rep)
  def |(x : ULong) = ULong(rep | x.rep)
  def ^(x : ULong) = ULong(rep ^ x.rep)

  def <<(x : Int)(implicit d: DummyImplicit) = ULong(rep << x)
  def <<(x : Long)(implicit d: DummyImplicit) = ULong(rep << x)
  def <<(x : UInt) = ULong(rep << (x.rep & 0x3f))
  def <<(x : ULong) = ULong(rep << (x.rep & 0x3f))

  def >>(x : Int)(implicit d: DummyImplicit) = ULong(rep >>> x)
  def >>(x : Long)(implicit d: DummyImplicit) = ULong(rep >>> x)
  def >>(x : UInt) = ULong(rep >>> (x.rep & 0x3f))
  def >>(x : ULong) = ULong(rep >>> (x.rep & 0x3f))

  def >>>(x : Int)(implicit d: DummyImplicit) = ULong(rep >>> x)
  def >>>(x : Long)(implicit d: DummyImplicit) = ULong(rep >>> x)
  def >>>(x : UInt) = ULong(rep >>> (x.rep & 0x3f))
  def >>>(x : ULong) = ULong(rep >>> (x.rep & 0x3f))

  def unary_+ = this
  def unary_- = ULong(-rep)
  def unary_~ = ULong(~rep)
}

object ULong {
  def MinValue = ULong(0L)
  def MaxValue = ULong(~0L)
}
