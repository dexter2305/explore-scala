package examples

import org.scalatest._
import org.scalatest.events._
import org.scalatest.funsuite.AnyFunSuite

class MUnitLikeReporter extends Reporter {
  // ANSI escape codes for colors
  private val ANSI_GREEN = "\u001B[32m"
  private val ANSI_RED = "\u001B[31m"
  private val ANSI_ORANGE = "\u001B[33m"
  private val ANSI_RESET = "\u001B[0m"

  override def apply(event: Event): Unit = event match {
    case TestSucceeded(_, _, _, _, testName, _, _, duration, formatter, _, _, _, _, _) =>
      println(s"${ANSI_GREEN}    + $testName ($duration) ${ANSI_RESET}")
    case TestFailed(_, message, _, _, _, testName, _, _, analysis, _, duration, formatter, _, _, _, _, _) =>
      println(s"${ANSI_RED}==> X $testName\n    Reason: ${analysis.mkString(" ")} $message ${ANSI_RESET}")
    case TestIgnored(_, _, _, _, testName, testText, formatter, _, _, _, _) =>
      println(s"${ANSI_ORANGE}==> i $testName${ANSI_RESET}")
    case SuiteCompleted(
          ordinal,
          suiteName,
          suiteId,
          suiteClassName,
          duration,
          formatter,
          location,
          rerunner,
          payload,
          threadName,
          timeStamp
        ) =>
      println(s"My tests completed.")
    case _ =>
  }
}
object MUnitLikeReporter:
  def apply: MUnitLikeReporter = new MUnitLikeReporter

class CommonFunSuite extends AnyFunSuite:
  override def run(testName: Option[String], args: Args): Status =
    super.run(testName, args.copy(reporter = new MUnitLikeReporter))

class MathOperationFunsuite extends CommonFunSuite:
  test("basic test"):
    assert(1 == 1)

  test("basic test to fail"):
    assert(1 == 2)

  ignore("basic test to ignore"):
    assert(1 == 1)
