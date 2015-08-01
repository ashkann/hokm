lazy val root = (project in file(".")).settings(
  name := "Hokm",
  version := "1.0",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-unchecked", "-deprecation"),
//fork := true,
javaOptions ++= Seq("-Dfile.encoding=UTF-8")
)
