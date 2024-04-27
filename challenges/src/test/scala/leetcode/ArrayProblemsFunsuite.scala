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

  test("238. Product of array except self."):
    val testcases = Table(
      ("nums", "expected"),
      (List(1, 2, 3, 4), List(24, 12, 8, 6)),
      (List(-1, 1, 0, -3, 3), List(0, 0, 9, 0, 0))
    )
    forAll(testcases): (nums, expected) =>
      assert(ArrayProblems.productExceptSelf(nums.toArray) === expected)

  test("283. Move zeroes."):
    val testcases = Table(
      ("array", "expected"),
      (List(0, 1, 0, 3, 12), List(1, 3, 12, 0, 0)),
      (List(0), List(0)),
      (List(1, 2, 3), List(1, 2, 3)),
      (List(1), List(1)),
      (List(0, 0, 0, 0, 5), List(5, 0, 0, 0, 0))
    )
    forAll(testcases): (ints, expected) =>
      val array = ints.toArray
      ArrayProblems.moveZeroes(array)
      assert(array.toList === expected)

  test("66. Plus One."):
    val testcases = Table(
      ("digits", "expected"),
      (List(1, 2, 3), List(1, 2, 4)),
      (List(1), List(2)),
      (List(9), List(1, 0)),
      (List(4, 3, 2, 1), List(4, 3, 2, 2)),
      (List.empty[Int], List(1))
    )
    forAll(testcases): (digits, expected) =>
      assert(ArrayProblems.plusOne(digits.toArray).toList === expected)

  test("1822. Sign of the product of the array."):
    val testcases = Table(
      ("array", "expected"),
      (List(-1, -2, -3, -4, 3, 2, 1), 1),
      (List(1, 5, 0, 2, -3), 0),
      (List(-1, 1, -1, 1, -1), -1),
      (
        List(41, 65, 14, 80, 20, 10, 55, 58, 24, 56, 28, 86, 96, 10, 3, 84, 4, 41, 13, 32, 42, 43,
          83, 78, 82, 70, 15, -41),
        -1
      ),
      (List(3), 1)
    )
    forAll(testcases): (ints, expected) =>
      assert(ArrayProblems.arraySign(ints.toArray) === expected)
