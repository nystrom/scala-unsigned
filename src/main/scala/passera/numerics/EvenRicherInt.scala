package passera.numerics

class EvenRicherInt(x: Int) extends Proxy {
  def self: Any = x
  def to(y: Long, step: Long = 1L) = x.toLong to y by step
  def until(y: Long, step: Long = 1L) = x.toLong until y by step
  def max(y: Long) = x.toLong max y
  def min(y: Long) = x.toLong min y
}
