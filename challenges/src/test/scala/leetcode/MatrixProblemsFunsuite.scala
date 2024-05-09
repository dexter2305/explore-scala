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
      (List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)), List(1, 2, 3, 6, 9, 8, 7, 4, 5))
    )
    forAll(testcases): (matrix, expected) =>
      assertResult(expected):
        MatrixProblems.spiralOrder(matrix.map(_.toArray).toArray)

  test("73. Set matrix zeroes."):
    val testcases = Table(
      ("matrix", "exp"),
      (
        List(List(1, 1, 1), List(1, 0, 1), List(1, 1, 1)),
        List(List(1, 0, 1), List(0, 0, 0), List(1, 0, 1))
      ),
      (
        List(List(0, 1, 2, 0), List(3, 4, 5, 2), List(1, 3, 1, 5)),
        List(List(0, 0, 0, 0), List(0, 4, 5, 0), List(0, 3, 1, 0))
      )
    )
    forAll(testcases): (matrix, expected) =>
      val matrixArray = matrix.map(_.toArray).toArray
      MatrixProblems.setZeroes(matrixArray)
      assertResult(expected)(matrixArray.map(_.toList).toList)
