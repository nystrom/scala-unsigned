package passera

package object numerics {
  implicit def toRicherInt(x: Int): RicherInt = new RicherInt(x)
  implicit def toRicherInt(x: scala.runtime.RichInt) = new EvenRicherInt(x.self.asInstanceOf[Int])

  implicit def toRicherLong(x: Long): RicherLong = new RicherLong(x)
  implicit def toRicherLong(x: scala.runtime.RichLong) = new EvenRicherLong(x.self.asInstanceOf[Long])
}
