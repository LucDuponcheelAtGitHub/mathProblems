val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mathProblems",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.9" % Test),

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
