package examples

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class VoterServiceFunsuite extends AnyFunSuite with ScalaCheckPropertyChecks:

  test("canVote"):
    val testcases = Table(
      ("age", "can vote"),
      (0, false),
      (17, false)
    )
    forAll(testcases): (age, expected) =>
      assert(VoterService.canVote(age) == expected)

  test("votingWard"):
    val testcases = Table(
      ("voter id", "ward number"),
      ("a", "ward 1")
    )
    forAll(testcases): (id, wardNumber) =>
      assert(VoterService.votingWard(id) === wardNumber)
