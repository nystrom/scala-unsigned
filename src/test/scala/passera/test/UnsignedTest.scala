package passera.test

import org.scalacheck._
import org.scalacheck.Prop._
import passera.Unsigned._

object UnsignedTest {

  object UnsignedSpecification extends Properties("Unsigned") {
    import Gen._
    import Arbitrary.arbitrary

    val zero = 0.toUInt
    val one = 1.toUInt

    def genUInt: Gen[UInt] = for (n <- arbitrary[Int]) yield UInt(n)
    implicit def arbUInt: Arbitrary[UInt] = Arbitrary(genUInt)

    property("int-toString") =
      forAll { n: Int => n >= 0 ==> (n.toUInt.toString == n.toString) }

    val nonNegLong = Gen.choose(0L, 0x00000000ffffffffL)
    property("long-toString") =
      forAll(nonNegLong){ n => n.toUInt.toString == n.toString }

    property("toUInt->toInt inverses") =
      forAll { (a: Int) => a.toUInt.toInt == a }
    property("toInt->toUInt inverses") =
      forAll { (a: UInt) => a.toInt.toUInt == a }
    property("toUInt-toDouble") =
    forAll { (a: Int) => a.toUInt.toDouble == a.toDouble }
    property(">= 0") =
    forAll { (a: UInt) => a >= zero }
    property("+ commutes") =
    forAll { (a: UInt, b: UInt) => a + b == b + a }
    property("* commutes") =
    forAll { (a: UInt, b: UInt) => a * b == b * a }
    property("zero identity for +") =
    forAll { (a: UInt, b: UInt) => a + zero == a }
    property("one identity for *") =
    forAll { (a: UInt, b: UInt) => a * one == a }
    property("zero is zero *") =
    forAll { (a: UInt, b: UInt) => a * zero == zero }
    property("+ associates") =
    forAll { (a: UInt, b: UInt, c: UInt) => a + (b + c) == (a + b) + c }
    property("* distributes left") =
    forAll { (a: UInt, b: UInt, c: UInt) => a * (b + c) == (a*b) + (b*c) }
    property("* distributes right") =
    forAll { (a: UInt, b: UInt, c: UInt) => (a + b) * c == (a*c) + (b*c) }
  }

  def main(args: Array[String]) = UnsignedSpecification.main(args)
}
