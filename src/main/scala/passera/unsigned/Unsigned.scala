package passera.unsigned

import Ops.{UnsignedOpWorkaround, SignedOpWorkaround}

trait Unsigned[U <: Unsigned[U, Promoted, SignedPromoted],
               Promoted <: Unsigned[_, Promoted, SignedPromoted],
               SignedPromoted]
  extends Any
{

  def toUByte: UByte
  def toUShort: UShort
  def toUInt: UInt
  def toULong: ULong

  def byteValue: Byte
  def shortValue: Short
  def intValue: Int
  def longValue: Long
  def floatValue: Float
  def doubleValue: Double

  // Implementing ScalaNumber
  protected def isWhole: Boolean = true
  def underlying = this

  def toChar = intValue.toChar
  def toByte = byteValue
  def toShort = shortValue
  def toInt = intValue
  def toLong = longValue
  def toFloat = floatValue
  def toDouble = doubleValue

  def +(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted
  def -(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted
  def *(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted
  def /(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted
  def %(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted
  def &(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted
  def |(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted
  def ^(x: SignedPromoted)(implicit ignore: SignedOpWorkaround): SignedPromoted

  def +(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def -(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def *(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def /(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def %(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def &(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def |(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def ^(x: UByte)(implicit ignore: UnsignedOpWorkaround): Promoted
  def <(x: UByte): Boolean
  def >(x: UByte): Boolean
  def <=(x: UByte): Boolean
  def >=(x: UByte): Boolean

  def +(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def -(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def *(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def /(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def %(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def &(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def |(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def ^(x: UShort)(implicit ignore: UnsignedOpWorkaround): Promoted
  def <(x: UShort): Boolean
  def >(x: UShort): Boolean
  def <=(x: UShort): Boolean
  def >=(x: UShort): Boolean

  def +(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def -(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def *(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def /(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def %(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def &(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def |(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def ^(x: ULong)(implicit ignore: UnsignedOpWorkaround): ULong
  def <(x: ULong): Boolean
  def >(x: ULong): Boolean
  def <=(x: ULong): Boolean
  def >=(x: ULong): Boolean

  def +(x: UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def -(x: UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def *(x: UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def /(x: UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def %(x: UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def &(x : UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def |(x : UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def ^(x : UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def <(x: UInt): Boolean
  def >(x: UInt): Boolean
  def <=(x: UInt): Boolean
  def >=(x: UInt): Boolean

  def unary_+ : Promoted
  def unary_- : Promoted

  // Equality comparison to UInt is baked in

  def unary_~ : Promoted

  def <<(x : Int)(implicit ignore: SignedOpWorkaround): Promoted
  def <<(x : Long)(implicit ignore: SignedOpWorkaround): Promoted
  def >>(x : Long)(implicit ignore: SignedOpWorkaround): Promoted
  def >>(x : Int)(implicit ignore: SignedOpWorkaround): Promoted
  def >>>(x : Int)(implicit ignore: SignedOpWorkaround): Promoted
  def >>>(x : Long)(implicit ignore: SignedOpWorkaround): Promoted

  def <<(x : UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def <<(x : ULong)(implicit ignore: UnsignedOpWorkaround): Promoted
  def >>(x : UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def >>(x : ULong)(implicit ignore: UnsignedOpWorkaround): Promoted
  def >>>(x : UInt)(implicit ignore: UnsignedOpWorkaround): Promoted
  def >>>(x : ULong)(implicit ignore: UnsignedOpWorkaround): Promoted

  override def toString: String

  def +(x : java.lang.String): String

  def toHexString: String
  def toOctalString: String
  def toBinaryString: String
}
