package passera.unsigned

class UShort(override val shortValue: Short) extends AnyVal with SmallUInt[UShort] {
  override def intValue = shortValue & 0xffff
}

object UShort {
  def MinValue = UShort(0)
  def MaxValue = UShort(~0)

  def apply(v: Short) = new UShort(v)
}
