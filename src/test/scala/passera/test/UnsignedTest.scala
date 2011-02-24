package passera.test

import org.scalacheck._
import org.scalacheck.Prop._
import passera.unsigned._

object UnsignedTest {

  object UnsignedSpecification extends Properties("Unsigned") {
    import Gen._
    import Arbitrary.arbitrary

    val zero = 0.toUInt
    val one = 1.toUInt

    def genUInt: Gen[UInt] = for (n <- arbitrary[Int]) yield UInt(n)
    implicit def arbUInt: Arbitrary[UInt] = Arbitrary(genUInt)

    val nonNegInt = Gen.choose(0, Int.MaxValue)
    val nonNegLong = Gen.choose(0L, 0x00000000ffffffffL)

    property("UIntEqualsInt") =
      forAll(nonNegInt) { n => (n.toUInt == n) }
    property("IntEqualsUInt") =
      forAll(nonNegInt) { n => (n == n.toUInt) }
    property("UIntNotNotEqualsInt") =
      forAll(nonNegInt) { n => !(n.toUInt != n) }
    property("IntNotNotEqualsUInt") =
      forAll(nonNegInt) { n => !(n != n.toUInt) }

    property("SameToStringAsInt") =
      forAll(nonNegInt) { n => n.toUInt.toString == (n.toString + "u") }

    property("SameToStringAsLong") =
      forAll(nonNegLong) { n => n.toUInt.toString == (n.toString + "u") }

    property("ToUIntAndToIntInverses") =
      forAll { (a: Int) => a.toUInt.toInt == a }
    property("ToIntToUIntInverses") =
      forAll { (a: UInt) => a.toInt.toUInt == a }
    property("ToUIntToDoubleEqualsToDouble") =
      forAll(nonNegInt) { a => (a.toUInt.toDouble == a.toDouble) }
    property("ToUIntToLongEqualsToLong") =
      forAll(nonNegInt) { a => (a.toUInt.toLong == a.toLong) }
    property("GreaterThan0") =
      forAll { (a: UInt) => a >= zero }
    property("PlusCommutes") =
      forAll { (a: UInt, b: UInt) => a + b == b + a }
    property("MultCommutes") =
      forAll { (a: UInt, b: UInt) => a * b == b * a }
    property("ZeroIdentityForPlus") =
      forAll { (a: UInt) => a + zero == a }
    property("ZeroIntIdentityForPlus") =
      forAll { (a: UInt) => a + 0 == a.toInt }
    property("OneIdentityForMult") =
      forAll { (a: UInt) => a * one == a }
    property("OneIntIdentityForMult") =
      forAll { (a: UInt) => a * 1 == a.toInt }
    property("ZeroIsZeroForMult") =
      forAll { (a: UInt) => a * zero == zero }
    property("ZeroIntIsZeroForMult") =
      forAll { (a: UInt) => a * 0 == 0 }
    property("PlusAssociates") =
      forAll { (a: UInt, b: UInt, c: UInt) => a + (b + c) == (a + b) + c }
    property("MultDistributesLeft") =
      forAll { (a: UInt, b: UInt, c: UInt) => a * (b + c) == (a*b) + (a*c) }
    property("MultDistributesRight") =
      forAll { (a: UInt, b: UInt, c: UInt) => (a + b) * c == (a*c) + (b*c) }

    property("Minus1") =
      forAll { (a: UInt) => -a == zero - a }
    property("Minus2") =
      forAll { (a: UInt) => zero - a == -a }

    property("PlusMinusInverses1") =
      forAll { (a: UInt, b: UInt) => a - b + b == a }
    property("PlusMinusInverses2") =
      forAll { (a: UInt, b: UInt) => a + b - b == a }
    property("PlusMinusInverses3") =
      forAll { (a: UInt, b: UInt) => b + a - b == a }
    property("PlusMinusInverses4") =
      forAll { (a: UInt, b: UInt) => -b + a + b == a }
    property("PlusMinusInverses5") =
      forAll { (a: UInt, b: UInt) => -b + a + b == a }
    property("PlusMinusInverses6") =
      forAll { (a: UInt, b: UInt) => b + (a - b) == a }
    property("PlusMinusInverses7") =
      forAll { (a: UInt, b: UInt) => b + (-b + a) == a }

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

    property("LessThan") =
      forAll { (a: Int, b: Int) => (a >= 0 && b >= 0) ==> (a >= b || a.toUInt < b.toUInt) }
    property("GreaterThan") =
      forAll { (a: Int, b: Int) => (a >= 0 && b >= 0) ==> (a <= b || a.toUInt > b.toUInt) }
    property("LessThanOrEqual") =
      forAll { (a: Int, b: Int) => (a >= 0 && b >= 0) ==> (a > b || a.toUInt <= b.toUInt) }
    property("GreaterThanOrEqual") =
      forAll { (a: Int, b: Int) => (a >= 0 && b >= 0) ==> (a < b || a.toUInt >= b.toUInt) }
    property("Equals") =
      forAll { (a: UInt) => a == a }
    property("NotEqualsPlusOne") =
      forAll { (a: UInt) => a+one != a }
    property("NotEquals") =
      forAll { (a: Int, b: Int) => (a != b) ==> (a.toUInt != b.toUInt) }
  }

  def main(args: Array[String]) = UnsignedSpecification.main(args)
}
