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

  test("463. Island perimter"):
    val testcases = Table(
      ("grid", "expected perimeter"),
      (Array(Array(0, 1, 0, 0), Array(1, 1, 1, 0), Array(0, 1, 0, 0), Array(1, 1, 0, 0)), 16),
      (Array(Array(1)), 4),
      (Array(Array(1, 0)), 4)
    )
    forAll(testcases): (grid, expected) =>
      assert(ArrayProblems.islandPerimeter(grid) === expected)
