package leetcode

object ArrayProblems:

  /** 2073. Time needed to buy tickets.
    *
    * There are n people in a line queuing to buy tickets, where the 0th person is at the front of
    * the line and the (n - 1)th person is at the back of the line.
    *
    * You are given a 0-indexed integer array tickets of length n where the number of tickets that
    * the ith person would like to buy is tickets[i].
    *
    * Each person takes exactly 1 second to buy a ticket. A person can only buy 1 ticket at a time
    * and has to go back to the end of the line (which happens instantaneously) in order to buy more
    * tickets. If a person does not have any tickets left to buy, the person will leave the line.
    *
    * Return the time taken for the person at position k (0-indexed) to finish buying tickets.
    *
    * Example 1:
    *
    * Input: tickets = [2,3,2], k = 2 Output: 6 Explanation: \- In the first pass, everyone in the
    * line buys a ticket and the line becomes [1, 2, 1]. \- In the second pass, everyone in the line
    * buys a ticket and the line becomes [0, 1, 0]. The person at position 2 has successfully bought
    * 2 tickets and it took 3 + 3 = 6 seconds.
    *
    * Example 2:
    *
    * Input: tickets = [5,1,1,1], k = 0 Output: 8 Explanation: \- In the first pass, everyone in the
    * line buys a ticket and the line becomes [4, 0, 0, 0]. \- In the next 4 passes, only the person
    * in position 0 is buying tickets. The person at position 0 has successfully bought 5 tickets
    * and it took 4 + 1 + 1 + 1 + 1 = 8 seconds.
    *
    * Constraints:
    *
    * n == tickets.length 1 <= n <= 100 1 <= tickets[i] <= 100 0 <= k < n
    *
    * Approach
    *
    * Solution to be approached in a single pass Every ticket bought is ONE second. Question is to
    * find the time required. Time required will be equal to the number of tickets sold until kth
    * person has no more tickets to buy.
    *
    * Everyone in front of k will either buy k or less than k. Everyone after k will buy on
    * tickets(k) - 1 tickets as the counting stops after k == 0.
    */
  def timeRequiredToBuy(tickets: Array[Int], k: Int): Int =
    (0 until tickets.length).foldLeft(0): (acc, index) =>
      if index <= k then tickets(index).min(tickets(k)) + acc
      else tickets(index).min(tickets(k) - 1) + acc

    /** 463. Island perimeter.
      *
      * You are given row x col grid representing a map where grid[i][j] = 1 represents land and
      * grid[i][j] = 0 represents water.
      *
      * Grid cells are connected horizontally/vertically (not diagonally). The grid is completely
      * surrounded by water, and there is exactly one island (i.e., one or more connected land
      * cells).
      *
      * The island doesn't have "lakes", meaning the water inside isn't connected to the water
      * around the island. One cell is a square with side length 1. The grid is rectangular, width
      * and height don't exceed 100. Determine the perimeter of the island.
      *
      * Example 1:
      *
      * Input: grid = [[0,1,0,0],[1,1,1,0],[0,1,0,0],[1,1,0,0]] Output: 16 Explanation: The
      * perimeter is the 16 yellow stripes in the image above.
      *
      * Example 2:
      *
      * Input: grid = [[1]] Output: 4
      *
      * Example 3:
      *
      * Input: grid = [[1,0]] Output: 4 Constraints:
      *
      * row == grid.length col == grid[i].length 1 <= row, col <= 100 grid[i][j] is 0 or 1. There is
      * exactly one island in grid.
      */
  def islandPerimeter(grid: Array[Array[Int]]): Int =

    val rRange = 0 until grid.length
    val cRange = 0 until grid(0).length

    /* pretty print given grid with a comment (debugging purpose)*/
    def pprint(grid: Array[Array[Int]], comment: Option[String] = None): Unit =
      val values = (for r <- rRange
      yield (for c <- cRange
      yield s"${grid(r)(c)}").toList).toList
      comment match
        case None => println("Matrix !!")
        case Some(string) =>
          println(string)
          println("*" * string.length())
      values.foreach: list =>
        print(list.mkString(" "))
        println

    /* given r,c - returns number of neighbouring islands */
    def neighboursFor(row: Int, col: Int): Int =
      val neighbours = List((row, col - 1), (row, col + 1), (row - 1, col), (row + 1, col))
      val present = neighbours
        .filter: (x, y) =>
          rRange.contains(x) && cRange.contains(y)
        .map(point => grid(point._1)(point._2))
        .sum
      present

    // pprint(grid = grid, Option("Given input"))
    val ps =
      for r <- rRange
      yield for c <- cRange
      yield
        if grid(r)(c) == 0 then 0
        else 4 - neighboursFor(r, c)
    ps.map(_.sum).sum

  /** 1431. Kids with greatest number of candies.
    *
    * There are n kids with candies. You are given an integer array candies, where each candies[i]
    * represents the number of candies the ith kid has, and an integer extraCandies, denoting the
    * number of extra candies that you have.
    *
    * Return a boolean array result of length n, where result[i] is true if, after giving the ith
    * kid all the extraCandies, they will have the greatest number of candies among all the kids, or
    * false otherwise.
    *
    * Note that multiple kids can have the greatest number of candies.
    *
    * Example 1:
    *
    * Input: candies = [2,3,5,1,3], extraCandies = 3 Output: [true,true,true,false,true]
    * Explanation: If you give all extraCandies to:
    *   - Kid 1, they will have 2 + 3 = 5 candies, * which is the greatest among the kids.
    *   - Kid 2, they will have 3 + 3 = 6 candies, which is the greatest among the kids.
    *   - Kid 3, they will have 5 + 3 = 8 candies, which is the greatest among the kids.
    *   - Kid 4, they will have 1 + 3 = 4 candies, which is not the greatest among the kids.
    *   - Kid 5, they will have 3 + 3 = 6 candies, which is the greatest among the kids.
    *
    * Example 2:
    *
    * Input: candies = [4,2,1,1,2], extraCandies = 1 Output: [true,false,false,false,false]
    * Explanation: There is only 1 extra candy. Kid 1 will always have the greatest number of
    * candies, even if a different kid is given the extra candy.
    *
    * Example 3:
    *
    * Input: candies = [12,1,12], extraCandies = 10 Output: [true,false,true]
    *
    * Constraints:
    *
    * n == candies.length 2 <= n <= 100 1 <= candies[i] <= 100 1 <= extraCandies <= 50
    */

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): List[Boolean] =
    val max = candies.max
    candies.map(_ + extraCandies >= max).toList
