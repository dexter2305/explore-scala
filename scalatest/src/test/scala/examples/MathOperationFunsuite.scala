package examples

import org.scalatest._
import org.scalatest.events._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class MUnitLikeReporter extends Reporter {
  // ANSI escape codes for colors
  private val ANSI_GREEN = "\u001B[32m"
  private val ANSI_RED = "\u001B[31m"
  private val ANSI_ORANGE = "\u001B[33m"
  private val ANSI_RESET = "\u001B[0m"

  val durationAsString: Option[Long] => String = (od) => if od.isDefined then s"(${od.get.toDouble / 1000}s)" else ""

  override def apply(event: Event): Unit = event match {
    case TestSucceeded(
          ordinal,
          suiteName,
          suiteId,
          suiteClassName,
          testName,
          testText,
          recordedEvents,
          duration,
          formatter,
          location,
          rerunner,
          payload,
          threadName,
          timestamp
        ) =>
      println(s"${ANSI_GREEN}    + $testName ${durationAsString(duration)} ${ANSI_RESET}")
    case TestFailed(
          ordinal,
          oMessage,
          suiteName,
          suiteId,
          oSuiteClassName,
          testName,
          testTexts,
          recordedEvents,
          analysis,
          throwables,
          duration,
          oFormatter,
          oLocation,
          oReRunner,
          payload,
          threadName,
          timestamp
        ) =>
      val ots = throwables match
        case Some(t) =>
          t.getStackTrace()
            .find(stacktraceElement => stacktraceElement.getClassName() == suiteId) match
            case Some(stacktraceelement) =>
              s"(${stacktraceelement.getClassName()}:${stacktraceelement.getLineNumber()})"
            case None => ""
        case None => ""
      println(
        s"${ANSI_RED}==> X $testName ${durationAsString(duration)}\n    ${analysis
            .mkString(" ")} $oMessage  ${ots} ${ANSI_RESET}"
      )
    case TestIgnored(
          ordinal,
          suiteName,
          suiteId,
          suiteClassName,
          testName,
          testText,
          formatter,
          location,
          payload,
          threadName,
          timeStamp
        ) =>
      println(s"${ANSI_ORANGE}==> i $testName${ANSI_RESET}")
    case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
      println("===> ")
    case x =>
    // println(s"\n${x.toString}\n")
  }
}
object MUnitLikeReporter:
  def apply: MUnitLikeReporter = new MUnitLikeReporter

class CommonFunSuite extends AnyFunSuite:
  override def run(testName: Option[String], args: Args): Status =
    val args = Args(reporter = new MUnitLikeReporter)
    super.run(testName, args)

// class MathOperationFunsuite extends CommonFunSuite with TableDrivenPropertyChecks:
class MathOperationFunsuite extends AnyFunSuite with TableDrivenPropertyChecks:
  test("basic test"):
    assert(1 == 1)

  test("basic test to fail"):
    assert(1 == 2)

  test("basic test to fail with ==="):
    assert(1 === 2)

  ignore("basic test to ignore"):
    assert(1 == 1)

  test("table driven checks"):
    val testcases = Table(
      ("a", "b", "expected"),
      (1, 2, 3),
      (3, 3, 2)
    )
    forAll(testcases): (a, b, expected) =>
      assert(a + b === expected)
