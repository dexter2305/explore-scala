package leetcode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ArrayProblemsFunsuite extends AnyFunSuite with ScalaCheckPropertyChecks:
  test("2073. Time needed to buy tickets"):
    val testcases = Table(
      ("tickets", "k", "expected"),
      (Array(2, 3, 2), 2, 6),
      (Array(5, 1, 1, 1), 0, 8)
    )
    forAll(testcases): (tickets, k, expected) =>
      assert(ArrayProblems.timeRequiredToBuy(tickets, k) === expected)
