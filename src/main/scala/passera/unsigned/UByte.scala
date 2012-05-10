package passera.unsigned

class UByte(override val byteValue: Byte) extends AnyVal with SmallUInt[UByte] {
  override def intValue = byteValue & 0xff
}

object UByte {
  def MinValue = UByte(0)
  def MaxValue = UByte(~0)

  def apply(v: Byte) = new UByte(v)
}
