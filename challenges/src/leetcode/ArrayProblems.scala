package leetcode

object ArrayProblems:

  /** 2073. Time needed to buy tickets.
    *
    * There are n people in a line queuing to buy tickets, where the 0th person is at the front of the line and the (n -
    * 1)th person is at the back of the line.
    *
    * You are given a 0-indexed integer array tickets of length n where the number of tickets that the ith person would
    * like to buy is tickets[i].
    *
    * Each person takes exactly 1 second to buy a ticket. A person can only buy 1 ticket at a time and has to go back to
    * the end of the line (which happens instantaneously) in order to buy more tickets. If a person does not have any
    * tickets left to buy, the person will leave the line.
    *
    * Return the time taken for the person at position k (0-indexed) to finish buying tickets.
    *
    * Example 1:
    *
    * Input: tickets = [2,3,2], k = 2 Output: 6 Explanation: \- In the first pass, everyone in the line buys a ticket
    * and the line becomes [1, 2, 1]. \- In the second pass, everyone in the line buys a ticket and the line becomes [0,
    * 1, 0]. The person at position 2 has successfully bought 2 tickets and it took 3 + 3 = 6 seconds.
    *
    * Example 2:
    *
    * Input: tickets = [5,1,1,1], k = 0 Output: 8 Explanation: \- In the first pass, everyone in the line buys a ticket
    * and the line becomes [4, 0, 0, 0]. \- In the next 4 passes, only the person in position 0 is buying tickets. The
    * person at position 0 has successfully bought 5 tickets and it took 4 + 1 + 1 + 1 + 1 = 8 seconds.
    *
    * Constraints:
    *
    * n == tickets.length 1 <= n <= 100 1 <= tickets[i] <= 100 0 <= k < n
    */
  def timeRequiredToBuy(tickets: Array[Int], k: Int): Int =
    /*
     * Solution to be approached in a single pass
     * Every ticket bought is ONE second. Question is to find the time required. Time required will be equal to the number of tickets sold until kth person has no more tickets to buy.
     *
     * Everyone in front of k will either buy k or less than k.
     * Everyone after k will buy on tickets(k) - 1 tickets as the counting stops after k == 0.
     *
     **/
    // var res = 0
    // for i <- 0 until tickets.length
    // do
    //   if i <= k then res = res + math.min(tickets(i), tickets(k))
    //   else res = res + math.min(tickets(i), tickets(k) - 1)
    // res
    (0 until tickets.length).foldLeft(0): (acc, index) =>
      val x =
        if index <= k then tickets(index).min(tickets(k)) + acc
        else tickets(index).min(tickets(k) - 1) + acc
      // println(s"for ${tickets.mkString("-")} at index = $index, tickets(index) = ${tickets(index)} vs ${tickets(k)}, x = $x")
      x