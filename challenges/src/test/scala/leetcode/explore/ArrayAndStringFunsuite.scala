package leetcode.explore

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ArrayAndStringFunsuite extends AnyFunSuite with ScalaCheckPropertyChecks:
  test("Find pivot index."):
    val testcases = Table(
      ("array", "expected"),
      (List(1, 7, 3, 6, 5, 6), 3),
      (List(1, 2, 3), -1),
      (List(2, 1, -1), 0)
    )
    forAll(testcases): (ints, expected) =>
      assertResult(expected):
        ArrayAndString.pivotIndex(ints.toArray)

  test("Largest number at least twice of others"):
    val testcases = Table(
      ("ints", "expected"),
      (List(3, 6, 1, 0), 1),
      (List(1, 2, 3, 4), -1)
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp)(ArrayAndString.dominantIndex(ints.toArray))

  test("Plus one.") {
    val testcases = Table(
      ("ints", "expected"),
      (List(1, 2, 3), List(1, 2, 4)),
      (List(4, 3, 2, 1), List(4, 3, 2, 2)),
      (List(9), List(1, 0))
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp):
        ArrayAndString.plusOne(ints.toArray).toList
  }
