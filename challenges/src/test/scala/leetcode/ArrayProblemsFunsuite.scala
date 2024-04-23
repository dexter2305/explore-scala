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

  test("1431. Kids with greatest number of candies"):
    val testcases = Table(
      ("candies", "extras", "expected"),
      (List(2, 3, 5, 1, 3), 3, List(true, true, true, false, true)),
      (List(4, 2, 1, 1, 2), 1, List(true, false, false, false, false)),
      (List(12, 1, 12), 1, List(true, false, true))
    )
    forAll(testcases): (candies, extras, expected) =>
      assert(ArrayProblems.kidsWithCandies(candies.toArray, extras) === expected)

  test("605. Can place flowers."):
    val testcases = Table(
      ("flowerbed", "extras", "expected"),
      (List(1, 0, 0, 0, 1), 1, true),
      (List(1, 0, 0, 0, 1), 2, false),
      (List(1, 0, 0, 0, 1), 0, true),
      (List(0, 0, 0, 0, 0), 2, true),
      (List(1), 1, false),
      (List(1, 0, 1, 0), 1, false),
      (List(1, 0, 0, 0), 1, true),
      (List(0, 1, 0), 1, false),
      (List(0), 1, true)
    )
    forAll(testcases): (bed, extra, expected) =>
      assert(ArrayProblems.canPlaceFlowers(bed.toArray, extra) === expected)
