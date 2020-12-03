libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "2.2.0",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)

// run := println(s"Project version: ${scalaVersion.value}")

Compile / run := {
  val scala = scalaVersion.value
  println(s"Scala version: $scala")
  (Compile / run).evaluated
  println(s"Task finished at: ${java.time.Instant.now()}")
}