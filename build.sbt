name := "interactional-justice-scala"

version := "1.0"

scalaVersion := "2.11.8"


libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
//  "org.scalacheck" %% "scalacheck" % "1.13.0" % "test",
  "org.scala-lang" % "scala-reflect" % "2.11.8",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
  "com.typesafe.akka" %% "akka-actor" % "2.4.7"
)
