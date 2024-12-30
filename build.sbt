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
        "org.scalatest" %% "scalatest-funsuite" % "3.2.19" % Test,
        "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
      ),
      Compile / scalacOptions := Seq(
        "-deprecation",
        "-Xfatal-warnings"
      ),
      Compile / doc / scalacOptions ++= Seq(
        "-groups"
      ),
      coverageFailOnMinimum := true,
      coverageMinimumStmtTotal := 90,
      coverageMinimumBranchTotal := 90
    )

lazy val challenges: Project = scala3Module(file("challenges"))
