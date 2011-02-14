package passera.text

object Test1 {
  import passera.Unsigned._

  def main(args: Array[String]) = {
    for (u <- 0.toUInt to 10.toUInt) {
      println(u)
    }

    exit(0)

    println(1e9.toLong.toUInt)
    println(2e9.toLong.toUInt)
    println(3e9.toLong.toUInt)
    println(4e9.toLong.toUInt)
    println(1e9.toLong.toUInt|(1).toUInt)
    println(2e9.toLong.toUInt|(1).toUInt)
    println(3e9.toLong.toUInt|(1).toUInt)
    println(4e9.toLong.toUInt|(1).toUInt)

    for (s <- List(1, 2, 4, 5, 8, 9, 10, 15, 16, 17, 22, 0x12345678)) {
        val u = s.toUInt
    for (t <- List(1, 2, 4, 5, 8, 9, 10, 15, 16, 17, 22, 0x12345678)) {
        val v = t.toUInt

        println("(" + u + " / " + v + ") = " + (u / v))
        println("(" + u + " * " + v + ") = " + (u * v))
        println("(" + u + " + " + v + ") = " + (u + v))
        println("(" + u + " - " + v + ") = " + (u - v))

        println("(" + u + " / " + t + ") = " + (u / t))
        println("(" + u + " * " + t + ") = " + (u * t))
        println("(" + u + " + " + t + ") = " + (u + t))
        println("(" + u + " - " + t + ") = " + (u - t))

        println("(" + u + " < " + v + ") = " + (u < v))
        println("(" + u + " > " + v + ") = " + (u > v))
        println("(" + u + " <= " + v + ") = " + (u <= v))
        println("(" + u + " >= " + v + ") = " + (u >= v))
        println("(" + u + " == " + v + ") = " + (u == v))
        println("(" + u + " != " + v + ") = " + (u != v))
    }
    }
    }
}
