package passera.unsigned

import scala.math.{ScalaNumber, ScalaNumericConversions}

@serializable
trait Unsigned[U <: Unsigned[U, Promoted, SignedPromoted], Promoted <: Unsigned[_, Promoted, SignedPromoted], SignedPromoted] extends ScalaNumericConversions {
  def toUByte: UByte
  def toUShort: UShort
  def toUInt: UInt
  def toULong: ULong

  override def byteValue: Byte
  override def shortValue: Short
  override def intValue: Int
  override def longValue: Long
  override def floatValue: Float
  override def doubleValue: Double

  // Implementing ScalaNumber
  protected def isWhole: Boolean = true
  def underlying = this

  def +(x: SignedPromoted): SignedPromoted
  def -(x: SignedPromoted): SignedPromoted
  def *(x: SignedPromoted): SignedPromoted
  def /(x: SignedPromoted): SignedPromoted
  def %(x: SignedPromoted): SignedPromoted
  def &(x: SignedPromoted): SignedPromoted
  def |(x: SignedPromoted): SignedPromoted
  def ^(x: SignedPromoted): SignedPromoted

  def +(x: UByte): Promoted
  def -(x: UByte): Promoted
  def *(x: UByte): Promoted
  def /(x: UByte): Promoted
  def %(x: UByte): Promoted
  def &(x: UByte): Promoted
  def |(x: UByte): Promoted
  def ^(x: UByte): Promoted
  def <(x: UByte): Boolean
  def >(x: UByte): Boolean
  def <=(x: UByte): Boolean
  def >=(x: UByte): Boolean

  def +(x: UShort): Promoted
  def -(x: UShort): Promoted
  def *(x: UShort): Promoted
  def /(x: UShort): Promoted
  def %(x: UShort): Promoted
  def &(x: UShort): Promoted
  def |(x: UShort): Promoted
  def ^(x: UShort): Promoted
  def <(x: UShort): Boolean
  def >(x: UShort): Boolean
  def <=(x: UShort): Boolean
  def >=(x: UShort): Boolean

  def +(x: ULong): ULong
  def -(x: ULong): ULong
  def *(x: ULong): ULong
  def /(x: ULong): ULong
  def %(x: ULong): ULong
  def &(x: ULong): ULong
  def |(x: ULong): ULong
  def ^(x: ULong): ULong
  def <(x: ULong): Boolean
  def >(x: ULong): Boolean
  def <=(x: ULong): Boolean
  def >=(x: ULong): Boolean

  def +(x: UInt): Promoted
  def -(x: UInt): Promoted
  def *(x: UInt): Promoted
  def /(x: UInt): Promoted
  def %(x: UInt): Promoted
  def &(x : UInt): Promoted
  def |(x : UInt): Promoted
  def ^(x : UInt): Promoted
  def <(x: UInt): Boolean
  def >(x: UInt): Boolean
  def <=(x: UInt): Boolean
  def >=(x: UInt): Boolean

  def unary_+ : Promoted
  def unary_- : Promoted

  // Equality comparison to UInt is baked in

  // Override equals to allow comparison with other number types.
  // By overriding ScalaNumber, we can cause UInt.equals to be invoked when
  // comparing a number on the left with a UInt on the right.
  // This is an (undocumented?) hack and might change in the future.
  override def equals(x: Any): Boolean
  def canEqual(x: Any): Boolean

  def unary_~ : Promoted

  def <<(x : Int): Promoted
  def <<(x : Long): Promoted
  def >>(x : Long): Promoted
  def >>(x : Int): Promoted
  def >>>(x : Int): Promoted
  def >>>(x : Long): Promoted
  def <<(x : UInt): Promoted
  def >>(x : UInt): Promoted
  def >>>(x : UInt): Promoted
  def <<(x : ULong): Promoted
  def >>(x : ULong): Promoted
  def >>>(x : ULong): Promoted

  override def toString: String

  def +(x : java.lang.String): String

  def toHexString: String
  def toOctalString: String
  def toBinaryString: String
}
