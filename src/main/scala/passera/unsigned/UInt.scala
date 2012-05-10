package passera.unsigned

class UInt(override val intValue: Int) extends AnyVal with SmallUInt[UInt] {
  override def toUInt = this
}

object UInt {
  def MinValue = UInt(0)
  def MaxValue = UInt(~0)

  def apply(v: Int) = new UInt(v)
}
