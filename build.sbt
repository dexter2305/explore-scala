import sbt.*

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / scalaVersion := "3.4.0"
ThisBuild / organization := "io.lyt"
ThisBuild / version := "0.0.1"

/** Builder for scala 3 module.
  *
  * Constructor for sbt modules with scala 3 compiler with scalatest.Funsuite in ~Test~ scope.
  * Additional options for scalac.
  */
def scala3Module(moduleDir: File): Project =
  Project(moduleDir.getName, moduleDir)
    .settings(
      name := moduleDir.getName,
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest-funsuite" % "3.2.18" % Test,
        "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test
      ),
      Compile / scalacOptions := Seq(
        "-deprecation",
        "-Xfatal-warnings"
      )
    )

lazy val challenges: Project = scala3Module(file("challenges"))
