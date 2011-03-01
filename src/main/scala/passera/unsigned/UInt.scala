package passera.unsigned

import scala.math.{ScalaNumber, ScalaNumericConversions}

@serializable
case class UInt(override val intValue: Int) extends ScalaNumber with SmallUInt {
  override def toUInt = this
}
