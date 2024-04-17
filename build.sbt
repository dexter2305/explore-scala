
ThisBuild / scalaVersion := "3.4.0"


lazy val challenges =
  (project in file("challenges"))
    .settings(
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest-funsuite" % "3.2.18",
        "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0"
      )
    )
