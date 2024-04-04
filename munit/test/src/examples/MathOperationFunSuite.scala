package examples
import munit._
class MathOperationFunsuite extends munit.FunSuite:
  test("basic test"):
    assert(1 == 1)

  test("basic test to fail"):
    assertEquals(1, 2)

  test("basic test to ignore".ignore):
    assert(1 == 1)

  test("tests with a bunch of data"):
    val testcases = List(
      (1, 2, 3),
      (1, 2, 3),
      (1, 2, 1)
    )
    for (a, b, expected) <- testcases do assertEquals(a + b, expected)
