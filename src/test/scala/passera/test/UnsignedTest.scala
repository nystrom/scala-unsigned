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

    property("UIntEqualsInt") =
      forAll { n: Int => n >= 0 ==> (n.toUInt == n) }
    property("IntEqualsUInt") =
      forAll { n: Int => n >= 0 ==> (n == n.toUInt) }
    property("UIntNotNotEqualsInt") =
      forAll { n: Int => n >= 0 ==> !(n.toUInt != n) }
    property("IntNotNotEqualsUInt") =
      forAll { n: Int => n >= 0 ==> !(n != n.toUInt) }

    property("SameToStringAsInt") =
      forAll { n: Int => n >= 0 ==> (n.toUInt.toString == (n.toString + "u")) }

    val nonNegLong = Gen.choose(0L, 0x00000000ffffffffL)
    property("SameToStringAsLong") =
      forAll(nonNegLong){ n => n.toUInt.toString == (n.toString + "u") }

    property("ToUIntAndToIntInverses") =
      forAll { (a: Int) => a.toUInt.toInt == a }
    property("ToIntToUIntInverses") =
      forAll { (a: UInt) => a.toInt.toUInt == a }
    property("ToUIntToDoubleEqualsToDouble") =
      forAll { (a: Int) => a >= 0 ==> (a.toUInt.toDouble == a.toDouble) }
    property("ToUIntToLongEqualsToLong") =
      forAll { (a: Int) => a >= 0 ==> (a.toUInt.toLong == a.toLong) }
    property("GreaterThan0") =
      forAll { (a: UInt) => a >= zero }
    property("PlusCommutes") =
      forAll { (a: UInt, b: UInt) => a + b == b + a }
    property("MultCommutes") =
      forAll { (a: UInt, b: UInt) => a * b == b * a }
    property("ZeroIdentityForPlus") =
      forAll { (a: UInt, b: UInt) => a + zero == a }
    property("OneIdentityForMult") =
      forAll { (a: UInt, b: UInt) => a * one == a }
    property("ZeroIsZeroForMult") =
      forAll { (a: UInt, b: UInt) => a * zero == zero }
    property("PlusAssociates") =
      forAll { (a: UInt, b: UInt, c: UInt) => a + (b + c) == (a + b) + c }
    property("MultDistributesLeft") =
      forAll { (a: UInt, b: UInt, c: UInt) => a * (b + c) == (a*b) + (a*c) }
    property("MultDistributesRight") =
      forAll { (a: UInt, b: UInt, c: UInt) => (a + b) * c == (a*c) + (b*c) }

    property("PlusMinusInverses1") =
      forAll { (a: UInt, b: UInt) => a - b + b == a.toInt }
    property("PlusMinusInverses2") =
      forAll { (a: UInt, b: UInt) => a + b - b == a.toInt }
    property("PlusMinusInverses3") =
      forAll { (a: UInt, b: UInt) => b + a - b == a.toInt }
    property("PlusMinusInverses4") =
      forAll { (a: UInt, b: UInt) => -b + a + b == a.toInt }
    property("PlusMinusInverses5") =
      forAll { (a: UInt, b: UInt) => -b + a + b == a.toInt }
    property("PlusMinusInverses6") =
      forAll { (a: UInt, b: UInt) => b + (a - b) == a.toInt }
    property("PlusMinusInverses7") =
      forAll { (a: UInt, b: UInt) => b + (-b + a) == a.toInt }

    property("SignedPlusMinusInverses1") =
      forAll { (a: Int, b: UInt) => a - b + b == a.toInt }
    property("SignedPlusMinusInverses2") =
      forAll { (a: Int, b: UInt) => a + b - b == a.toInt }
    property("SignedPlusMinusInverses3") =
      forAll { (a: Int, b: UInt) => b + a - b == a.toInt }
    property("SignedPlusMinusInverses4") =
      forAll { (a: Int, b: UInt) => -b + a + b == a.toInt }
    property("SignedPlusMinusInverses5") =
      forAll { (a: Int, b: UInt) => -b + a + b == a.toInt }
    property("SignedPlusMinusInverses6") =
      forAll { (a: Int, b: UInt) => b + (a - b) == a.toInt }
    property("SignedPlusMinusInverses7") =
      forAll { (a: Int, b: UInt) => b + (-b + a) == a.toInt }

    property("MultDivRem1") =
      forAll { (a: UInt, b: UInt, c: UInt) => b.toInt != 0 ==> (b * (a / b) + (a % b) == a) }

    property("MultDivRem2") =
      forAll { (a: UInt, b: UInt, c: UInt) => b.toInt != 0 ==> ((a / b) * b + (a % b) == a) }
  }

  def main(args: Array[String]) = UnsignedSpecification.main(args)
}
