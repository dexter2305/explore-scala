import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

trait GenericModule extends ScalaModule with ScalafmtModule {
  def scalaVersion: T[String] = "3.3.0"
  override def scalacOptions = Seq("-Xfatal-warnings")
}

object challenges extends GenericModule {

  object test extends ScalaTests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest-funsuite:3.2.18",
      ivy"org.scalatestplus::scalacheck-1-17:3.2.16.0"
    )
    def testFramework = "org.scalatest.tools.Framework"
    override def testCachedArgs = Seq("-oD")
  }
}

object scalatest extends GenericModule {

  lazy val scalatestVersion = "3.2.18"
  def moduleDeps = Seq(challenges)
  object test extends ScalaTests {
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest-funsuite:$scalatestVersion",
      ivy"org.scalatest::scalatest-flatspec:$scalatestVersion",
      ivy"org.scalatestplus::scalacheck-1-17:3.2.16.0"
    )
    def testFramework: T[String] = "org.scalatest.tools.Framework"
  }
}

object munit extends GenericModule {

  def moduleDeps = Seq(challenges)
  object test extends ScalaTests {
    def ivyDeps = Agg(
      ivy"org.scalameta::munit:0.7.29"
    )

    def testFramework: T[String] = "munit.Framework"
  }
}
