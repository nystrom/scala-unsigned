package passera.test

import passera.unsigned._

object UnsignedPerf {
  def main(args: Array[String]) = {

    def time(body: => Unit) = {
      var i = 0
      while (i < 10) {
        val t0 = System.nanoTime
        val r = body
        val t1 = System.nanoTime
        val d = t1 - t0
        i += 1
        println(d / 1e3 + " us")
      }
    }

    time {
      val n = 100000000
      val a = 328923.toUInt
      val b = 713480.toUInt
      var c = 0.toUInt

      var i = 0
      while (i < n) {
        i += 1
        c += a
        c *= b
        c /= a
      }

      c
    }

    time {
      val n = 100000000
      val a = 328923
      val b = 713480
      var c = 0

      var i = 0
      while (i < n) {
        i += 1
        c += a
        c *= b
        c /= a
      }

      c
    }
  }
}
