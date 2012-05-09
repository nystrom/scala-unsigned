package passera.unsigned

import scala.math.{ScalaNumber, ScalaNumericConversions}

@serializable
case class UByte(override val byteValue: Byte) extends ScalaNumber with SmallUInt[UByte] {
  override def intValue = byteValue & 0xff
}
