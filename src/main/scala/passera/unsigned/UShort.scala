package passera.unsigned

import scala.math.{ScalaNumber, ScalaNumericConversions}

@serializable
case class UShort(override val shortValue: Short) extends ScalaNumber with SmallUInt[UShort] {
  override def intValue = shortValue & 0xffff
}
