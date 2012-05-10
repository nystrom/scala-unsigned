package passera.numerics

class RicherLong(x: Long) extends Proxy {
  def self: Any = x

  import java.lang.Long

  def bitCount = Long.bitCount(x)
  def highestOneBit = Long.highestOneBit(x)
  def lowestOneBit = Long.lowestOneBit(x)
  def numberOfLeadingZeros = Long.numberOfLeadingZeros(x)
  def numberOfTrailingZeros = Long.numberOfTrailingZeros(x)

  def reverse = Long.reverse(x)
  def reverseBytes = Long.reverseBytes(x)
  def rotateLeft(dist: Int) = Long.rotateLeft(x, dist)
  def rotateRight(dist: Int) = Long.rotateRight(x, dist)
  def signum = Long.signum(x)

  def <<@(dist: Int) = rotateLeft(dist)
  def >>@(dist: Int) = rotateRight(dist)
}
