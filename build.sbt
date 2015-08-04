lazy val Hokm = (project in file(".")).settings(
  name := "Hokm",
  version := "1.0",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-unchecked", "-deprecation"),
  javaOptions ++= Seq("-Dfile.encoding=UTF-8"),
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)
