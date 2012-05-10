Passera
=======

This is an implementation of unsigned integers for Scala.
This is still very much a work in progress.  
Much of it was written a while ago as an exercise in learning Scala.

The library provides:
- UInt
- ULong
- UByte
- UShort

The classes are implemented by boxing Int, Long, Byte, Short, respectively.

To use:

    scala> import passera.unsigned._

    scala> (-1).toUInt
    res0: passera.unsigned.UInt = 4294967295

    scala> res0 + 1
    res1: Int = 0


Value classes
-------------

The branch value-classes contains a version that works with Scala 2.10.0-M3.
UInt is implemented as a value class. Because of this, it's more efficiently represented.
However, equality changes. Since value classes cannot override equals, we have
that 1.toUInt != 1. The built-in numbers implement ScalaNumber which (with some
implicit conversions) allows 1.toLong == 1.


Credits
-------

The code was written by Nate Nystrom (nate.nystrom@usi.ch).

TODO
----

Add support for value classes.

Refactor code to remove redundant ULong code.

More tests.
