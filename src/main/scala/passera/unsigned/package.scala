package passera

package object unsigned {
  import Ops._

  implicit object UByteOrdering extends UByteOrdering
  implicit object UShortOrdering extends UShortOrdering
  implicit object UIntOrdering extends UIntOrdering
  implicit object ULongOrdering extends ULongOrdering
  implicit object UByteIsIntegral extends UByteIsIntegral with UByteOrdering
  implicit object UShortIsIntegral extends UShortIsIntegral with UShortOrdering
  implicit object UIntIsIntegral extends UIntIsIntegral with UIntOrdering
  implicit object ULongIsIntegral extends ULongIsIntegral with ULongOrdering

  // implicit def s2u(x: Int) = UInt(x)
  // implicit def u2s(x: UInt) = x.rep

  // avoid name conflicts :-)
  implicit val ZnVja2luZyBmdWNraW5nIGhhY2sK: UnsignedOpWorkaround = new UnsignedOpWorkaround()
  implicit val YW5vdGhlciBmdWNraW5nIGhhY2sK: SignedOpWorkaround = new SignedOpWorkaround()

  implicit def ubyte2uint(x: UByte) = UInt(x.toInt)
  implicit def ushort2uint(x: UShort) = UInt(x.toInt)
  implicit def ubyte2ulong(x: UByte) = ULong(x.toLong)
  implicit def ushort2ulong(x: UShort) = ULong(x.toLong)
  implicit def uint2ulong(x: UInt) = ULong(x.toLong)

  implicit def signedIntOps(x: Int) = new SignedIntOps(x)
  implicit def signedLongOps(x: Long) = new SignedLongOps(x)
  implicit def floatOps(x: Float) = new FloatOps(x)
  implicit def doubleOps(x: Double) = new DoubleOps(x)
  implicit def signedRichIntOps(x: scala.runtime.RichInt) = new SignedRichIntOps(x.self.asInstanceOf[Int])
  implicit def richUInt(x: UInt) = new RichUInt(x)
  implicit def richerUInt(x: UInt) = new RicherUInt(x.toInt)
}

