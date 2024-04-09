package leetcode

object ArrayProblems:

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
      println(
        s"for ${tickets.mkString("-")} at index = $index, tickets(index) = ${tickets(index)} vs ${tickets(k)}, x = $x"
      )
      x
