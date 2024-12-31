package build

import mill._, scalalib._

object challenges extends SbtModule {

  override def scalaVersion = "3.5.0"

  object test extends SbtTests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest-funsuite:3.2.19",
      ivy"org.scalatestplus::scalacheck-1-18:3.2.19.0"
    )
    def testFramework = "org.scalatest.tools.Framework"
  }
}
