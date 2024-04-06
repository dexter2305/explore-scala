import sbt._

Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / scalaVersion := "3.4.0"
ThisBuild / organization := "io.lyt"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val challenges = (project in file("challenges"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest-funsuite" % "3.2.18" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test
    )
  )

lazy val scalatest = (project in file("scalatest"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest-funsuite" % "3.2.18" % Test,
      "org.scalatest" %% "scalatest-flatspec" % "3.2.18" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test
    )
  )
  .dependsOn(challenges)

lazy val munit = (project in file("munit"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
  .dependsOn(challenges)
