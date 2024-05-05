package leetcode

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import leetcode.LinkedListProblems.*
import org.scalatest.funsuite.AnyFunSuite

class LinkedListProblemsFunsuite extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("ListNode.asList"):
    val one = ListNode(1)
    val two = ListNode(2)
    val three = ListNode(3)
    assert(one.asList === List(1))
    one.next = two
    assert(one.asList === List(1, 2))
    two.next = three
    assert(one.asList === List(1, 2, 3))

  test("237. Delete a node in the linked list."):
    val four = ListNode(4)
    val five = ListNode(5)
    val one = ListNode(1)
    val nine = ListNode(9)
    four.next = five
    five.next = one
    one.next = nine
    deleteNode(five)
    assertResult(List(4, 1, 9))(four.asList)
}
