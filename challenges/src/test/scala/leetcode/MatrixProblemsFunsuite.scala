package leetcode

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MatrixProblemsFunsuite extends AnyFunSuite with ScalaCheckPropertyChecks:
  test("1572. Matrix diagonal sum."):
    val testcases = Table(
      ("matrix", "expected sum"),
      (
        List(
          List(1, 2, 3),
          List(4, 5, 6),
          List(7, 8, 9)
        ),
        25
      ),
      (
        List(List(1, 1, 1, 1), List(1, 1, 1, 1), List(1, 1, 1, 1), List(1, 1, 1, 1)),
        8
      ),
      (List(List(5)), 5)
    )
    forAll(testcases): (matrix, expected) =>
      assertResult(expected):
        MatrixProblems.diagonalSum(matrix.map(_.toArray).toArray)
  test("54. Spiral matrix."):
    val testcases = Table(
      ("array", "expected"),
      (List(List(1,2,3), List(4,5,6), List(7,8,9)), List(1,2,3,6,9,8,7,4,5))
    )
    forAll(testcases): (matrix, expected) =>
      assertResult(expected):
          MatrixProblems.spiralOrder(matrix.map(_.toArray).toArray)
