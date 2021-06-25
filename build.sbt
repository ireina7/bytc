val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "bytc",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
  )
