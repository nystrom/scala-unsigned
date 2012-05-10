package passera.numerics

class RicherInt(x: Int) extends Proxy {
  def self: Any = x

  import java.lang.Integer

  def bitCount = Integer.bitCount(x)
  def highestOneBit = Integer.highestOneBit(x)
  def lowestOneBit = Integer.lowestOneBit(x)
  def numberOfLeadingZeros = Integer.numberOfLeadingZeros(x)
  def numberOfTrailingZeros = Integer.numberOfTrailingZeros(x)

  def reverse = Integer.reverse(x)
  def reverseBytes = Integer.reverseBytes(x)
  def rotateLeft(dist: Int) = Integer.rotateLeft(x, dist)
  def rotateRight(dist: Int) = Integer.rotateRight(x, dist)
  def signum = Integer.signum(x)

  def <<@(dist: Int) = rotateLeft(dist)
  def >>@(dist: Int) = rotateRight(dist)
}
