//NOTE: `build.sc` is fixed at scala 2.13. Cannot use scala3 syntax. ScalaFmt might switch it to scala3.
import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill._
import mill.contrib.scoverage.ScoverageModule
import mill.scalalib._
import mill.scalalib.scalafmt._

trait Scala3Module extends ScoverageModule with ScalafmtModule {
  override def scalaVersion: T[String] = "3.3.0"
  override def scoverageVersion = "2.1.0"
  override def scalacOptions = Seq("-deprecation", "-Xfatal-warnings")
  override def scalaDocOptions = Seq("-no-link-warnings", "-groups")
  trait UnitTests extends ScalaTests with ScoverageTests {
    lazy val scalatestVersion = "3.2.18"
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest-funsuite:$scalatestVersion",
      ivy"org.scalatest::scalatest-flatspec:$scalatestVersion",
      ivy"org.scalatestplus::scalacheck-1-17:3.2.16.0"
    )
    def testFramework: T[String] = "org.scalatest.tools.Framework"
  }
}

object challenges extends Scala3Module {
  object test extends UnitTests
}

object scalatest extends Scala3Module {
  def moduleDeps = Seq(challenges)
  object test extends UnitTests
}

object munit extends Scala3Module {
  def moduleDeps = Seq(challenges)
  object test extends ScalaTests {
    def ivyDeps = Agg(
      ivy"org.scalameta::munit:0.7.29"
    )
    def testFramework: T[String] = "munit.Framework"
  }
}
