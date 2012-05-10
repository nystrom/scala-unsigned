name := "scala-unsigned"

version := "0.1"

scalaVersion := "2.10.0-M3"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" % "scalacheck_2.9.2" % "1.9" % "test"
)
