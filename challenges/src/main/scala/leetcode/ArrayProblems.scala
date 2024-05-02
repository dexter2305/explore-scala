package leetcode

import scala.compiletime.ops.double
import scala.annotation.meta.param

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
    *   - Input: tickets = [2,3,2], k = 2
    *   - Output: 6
    *   - Explanation:
    *     - In the first pass, everyone in the line buys a ticket and the line becomes [1, 2, 1].
    *     - In the second pass, everyone in the line buys a ticket and the line becomes [0, 1, 0].
    *       The person at position 2 has successfully bought 2 tickets and it took 3 + 3 = 6
    *       seconds.
    *
    * Example 2:
    *
    *   - Input: tickets = [5,1,1,1], k = 0
    *   - Output: 8
    *   - Explanation:
    *     - In the first pass, everyone in the line buys a ticket and the line becomes [4, 0, 0, 0].
    *     - In the next 4 passes, only the person in position 0 is buying tickets. The person at
    *       position 0 has successfully bought 5 tickets and it took 4 + 1 + 1 + 1 + 1 = 8 seconds.
    *
    * Constraints:
    *
    *   - n == tickets.length
    *   - 1 <= n <= 100
    *   - 1 <= tickets[i] <= 100
    *   - 0 <= k < n
    *
    * ##### Approach
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
      * ```
      * Example 1:
      *
      * - Input: grid = [[0,1,0,0],[1,1,1,0],[0,1,0,0],[1,1,0,0]]
      * - Output: 16
      * - Explanation: The perimeter is the 16 yellow stripes in the image above.
      *
      * Example 2:
      *
      * - Input: grid = [[1]]
      * - Output: 4
      *
      * Example 3:
      *
      * - Input: grid = [[1,0]]
      * - Output: 4
      * ```
      * Constraints:
      *   - row == grid.length
      *   - col == grid[i].length
      *   - 1 <= row, col <= 100
      *   - grid[i][j] is 0 or 1.
      *   - There is exactly one island in grid.
      *
      * @param Array[Array[Int]]
      * @return
      *   Int
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
    *   - Input: candies = [4,2,1,1,2], extraCandies = 1
    *   - Output: [true,false,false,false,false]
    *   - Explanation: There is only 1 extra candy. Kid 1 will always have the greatest number of
    *     candies, even if a different kid is given the extra candy.
    *
    * Example 3:
    *
    *   - Input: candies = [12,1,12], extraCandies = 10
    *   - Output: [true,false,true]
    *
    * Constraints:
    *
    *   - n == candies.length
    *   - 2 <= n <= 100
    *   - 1 <= candies[i] <= 100
    *   - 1 <= extraCandies <= 50
    */

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): List[Boolean] =
    val max = candies.max
    candies.map(_ + extraCandies >= max).toList

  /** 605. Can place flowers
    *
    * You have a long flowerbed in which some of the plots are planted, and some are not. However,
    * flowers cannot be planted in adjacent plots.
    *
    * Given an integer array flowerbed containing 0's and 1's, where 0 means empty and 1 means not
    * empty, and an integer n, return true if n new flowers can be planted in the flowerbed without
    * violating the no-adjacent-flowers rule and false otherwise.
    *
    * Example 1:
    *
    *   - Input: flowerbed = [1,0,0,0,1], n = 1
    *   - Output: true
    *
    * Example 2:
    *
    *   - Input: flowerbed = [1,0,0,0,1], n = 2
    *   - Output: false
    *
    * Constraints:
    *
    *   - 1 <= flowerbed.length <= 2 * 10^4
    *   - flowerbed[i] is 0 or 1. There are no two adjacent flowers in * flowerbed. 0 <= n <=
    *     flowerbed.length
    *
    * ##### Approach
    *
    *   - If the bed is List(0), 1 plant can be planted. This is true because of the assumption
    *     List(0) => [0]-List(0)-[0] => [0,0,0]
    *   - Going with this assumption, add head and tail to flowerbed with value = 0.
    *   - iterate from 1 until bed.length -1 - skip the first and last because of assumption.
    *   - now start matching if pointer == 0 and look for prev == next == 0.
    */
  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean =
    @scala.annotation.tailrec
    def iterate(bed: Array[Int], current: Int, terminal: Int, planted: Int): Boolean =
      if current <= terminal && planted < n then
        (bed(current - 1), bed(current), bed(current + 1)) match
          case (0, 0, 0) =>
            bed(current) = 1
            iterate(bed, current + 1, terminal, planted + 1)
          case _ =>
            iterate(bed, current + 1, terminal, planted)
      else planted == n
    iterate(0 +: flowerbed :+ 0, current = 1, terminal = flowerbed.length, 0)

  /** 238. Product of array except self.
    *
    * Given an integer array nums, return an array answer such that answer[i] is equal to the
    * product of all the elements of nums except nums[i].
    *
    * The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
    *
    * You must write an algorithm that runs in O(n) time and without using the division operation.
    *
    * Example 1:
    *
    *   - Input: nums = [1,2,3,4]
    *   - Output: [24,12,8,6]
    *
    * Example 2:
    *
    *   - Input: nums = [-1,1,0,-3,3]
    *   - Output: [0,0,9,0,0]
    *
    * Constraints:
    *
    *   - 2 <= nums.length <= 10^5
    *   - -30 <= nums[i] <= 30
    *   - The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
    *
    * ##### Approach
    *
    * Scan will include the extra starting 1 so there are 5 elements not a problem for prefix, but
    * it causes the indices to be off by 1 for suffixes, so I take the tail
    */
  def productExceptSelf(nums: Array[Int]): Array[Int] =
    val prefixProducts: Array[Int] = nums.scanLeft(1)(_ * _)
    val suffixProducts: Array[Int] = nums.scanRight(1)(_ * _).tail
    nums.indices
      .map(i => prefixProducts(i) * suffixProducts(i))
      .toArray

  /** 283. Move zeroes.
    *
    * Given an integer array nums, move all 0's to the end of it while maintaining the relative
    * order of the non-zero elements.
    *
    * Note that you must do this in-place without making a copy of the array.
    *
    * Example 1:
    *
    *   - Input: nums = [0,1,0,3,12]
    *   - Output: [1,3,12,0,0]
    *
    * Example 2:
    *
    *   - Input: nums = [0]
    *   - Output: [0]
    *
    * Constraints:
    *
    *   - 1 <= nums.length <= 10^4
    *   - -2^31 <= nums[i] <= 2^31 - 1
    *
    * Follow up: Could you minimize the total number of operations done?
    */
  def moveZeroes(nums: Array[Int]): Unit =
    @scala.annotation.tailrec
    def loop(first: Int, second: Int): Unit =
      if second < nums.length then
        (nums(first), nums(second)) match
          case (x @ 0, y @ 0) => loop(first, second + 1)
          case (x @ 0, y) if y != 0 =>
            nums(first) = nums(second)
            nums(second) = 0
            println(s"[${nums.mkString(",")}]")
            loop(first + 1, second + 1)
          case (x, y) if x != 0 && y != 0 => loop(first + 1, second + 1)
    loop(0, 0)

  /** 66. Plus one.
    *
    * You are given a large integer represented as an integer array digits, where each digits[i] is
    * the ith digit of the integer. The digits are ordered from most significant to least
    * significant in left-to-right order. The large integer does not contain any leading 0's.
    *
    * Increment the large integer by one and return the resulting array of digits.
    *
    * Example 1:
    *
    *   - Input: digits = [1,2,3]
    *   - Output: [1,2,4]
    *   - Explanation: The array represents the integer 123. Incrementing by one gives 123 + 1 =
    *     124. Thus, the result should be [1,2,4].
    *
    * Example 2:
    *
    *   - Input: digits = [4,3,2,1]
    *   - Output: [4,3,2,2]
    *   - Explanation: The array represents the integer 4321. Incrementing by one gives 4321 + 1 =
    *     4322. Thus, the result should be [4,3,2,2].
    *
    * Example 3:
    *
    *   - Input: digits = [9]
    *   - Output: [1,0]
    *   - Explanation: The array represents the integer 9. Incrementing by one gives 9 + 1 = 10.
    *     Thus, the result should be [1,0].
    *
    * Constraints:
    *
    *   - 1 <= digits.length <= 100
    *   - 0 <= digits[i] <= 9
    *   - digits does not contain any leading 0's.
    */
  def plusOne(digits: Array[Int]): Array[Int] =
    @scala.annotation.tailrec
    def loop(index: Int, carry: Int, acc: Array[Int]): Array[Int] =
      if index >= 0 then
        val sum = carry + digits(index)
        val q = sum / 10
        val r = sum % 10
        loop(index - 1, q, acc :+ r)
      else if carry > 0 then acc :+ carry
      else acc
    loop(digits.length - 1, 1, Array.emptyIntArray).reverse

  /** 1822. Sign of the product of an array.
    *
    * There is a function signFunc(x) that returns:
    *
    *   - 1 if x is positive.
    *   - -1 if x is negative.
    *   - 0 if x is equal to 0.
    *
    * You are given an integer array nums. Let product be the product of all values in the array
    * nums.
    *
    * Return signFunc(product).
    *
    * Example 1:
    *
    *   - Input: nums = [-1,-2,-3,-4,3,2,1]
    *   - Output: 1
    *   - Explanation: The product of all values in the array is 144, and signFunc(144) = 1
    *
    * Example 2:
    *
    *   - Input: nums = [1,5,0,2,-3]
    *   - Output: 0
    *   - Explanation: The product of all values in the array is 0, and signFunc(0) = 0
    *
    * Example 3:
    *
    *   - Input: nums = [-1,1,-1,1,-1]
    *   - Output: -1
    *   - Explanation: The product of all values in the array is -1, and signFunc(-1) = -1
    *
    * Constraints:
    *
    *   - 1 <= nums.length <= 1000
    *   - -100 <= nums[i] <= 100
    *
    * @param integers
    * @return
    *   - -1 when product < 0
    *   - 0 when product == 0
    *   - 1 when product > 0
    *
    * @note
    *   programming-skils
    */
  def arraySign(nums: Array[Int]): Int =
    @scala.annotation.tailrec
    def loop(index: Int, acc: Int): Int =
      if index == nums.length then acc
      else if nums(index) == 0 then 0
      else loop(index + 1, acc * (if nums(index) > 0 then 1 else -1))
    loop(0, 1)

  /** 1502. Can make arithmetic progression from sequence.
    *
    * A sequence of numbers is called an arithmetic progression if the difference between any two
    * consecutive elements is the same.
    *
    * Given an array of numbers arr, return true if the array can be rearranged to form an
    * arithmetic progression. Otherwise, return false.
    *
    * Example 1:
    *
    *   - Input: arr = [3,5,1]
    *   - Output: true
    *   - Explanation: We can reorder the elements as [1,3,5] or [5,3,1] with differences 2 and -2
    *     respectively, between each consecutive elements.
    *
    * Example 2:
    *
    *   - Input: arr = [1,2,4]
    *   - Output: false
    *   - Explanation: There is no way to reorder the elements to obtain an arithmetic progression.
    *
    * Constraints:
    *
    *   - 2 <= arr.length <= 1000
    *   - -10^6 <= arr[i] <= 10^6
    *
    * @param integers
    * @return
    *   true when AP can be formed from sequence
    *
    * @note
    *   programming-skills
    */
  def canMakeArithmeticProgression(arr: Array[Int]): Boolean =
    arr.sorted
      .sliding(2)
      .toList
      .map(pairAsList => pairAsList(1) - pairAsList(0))
      .distinct
      .size == 1

  /** 896. Monotonic array.
    *
    * An array is monotonic if it is either monotone increasing or monotone decreasing.
    *
    * An array nums is monotone increasing if for all i <= j, nums[i] <= nums[j]. An array nums is
    * monotone decreasing if for all i <= j, nums[i] >= nums[j].
    *
    * Given an integer array nums, return true if the given array is monotonic, or false otherwise.
    *
    * Example 1:
    *
    *   - Input: nums = [1,2,2,3]
    *   - Output: true
    *
    * Example 2:
    *
    *   - Input: nums = [6,5,4,4]
    *   - Output: true
    *
    * Example 3:
    *
    *   - Input: nums = [1,3,2]
    *   - Output: false
    *
    * Constraints:
    *
    *   - 1 <= nums.length <= 10^5
    *   - -10^5 <= nums[i] <= 10^5
    */
  def isMonotonic(nums: Array[Int]): Boolean =
    var isInc = false
    var isDec = false
    for i <- 1 until nums.length do
      if nums(i) > nums(i - 1) then isInc = true
      else if nums(i) < nums(i - 1) then isDec = true

    if isInc && isDec then false
    else true

  /** 682. Baseball game
    *
    * You are keeping the scores for a baseball game with strange rules. At the beginning of the game, you start with an empty record.
    *
    * You are given a list of strings operations, where operations[i] is the ith operation you must apply to the record and is one of the following:
    *
    *    An integer x.
    *        Record a new score of x.
    *    '+'.
    *        Record a new score that is the sum of the previous two scores.
    *    'D'.
    *        Record a new score that is the double of the previous score.
    *    'C'.
    *        Invalidate the previous score, removing it from the record.
    *
    * Return the sum of all the scores on the record after applying all the operations.
    *
    * The test cases are generated such that the answer and all intermediate calculations fit in a 32-bit integer and that all operations are valid.
    *
    * Example 1:
    *
    * - Input: ops = ["5","2","C","D","+"]
    * - Output: 30
    * - Explanation:
    *     - "5" - Add 5 to the record, record is now [5].
    *     - "2" - Add 2 to the record, record is now [5, 2].
    *     - "C" - Invalidate and remove the previous score, record is now [5].
    *     - "D" - Add 2 * 5 = 10 to the record, record is now [5, 10].
    *     - "+" - Add 5 + 10 = 15 to the record, record is now [5, 10, 15].
    *     The total sum is 5 + 10 + 15 = 30.
    *
    * Example 2:
    *
    * - Input: ops = ["5","-2","4","C","D","9","+","+"]
    * - Output: 27
    * - Explanation:
    *   - "5" - Add 5 to the record, record is now [5].
    *   - "-2" - Add -2 to the record, record is now [5, -2].
    *   - "4" - Add 4 to the record, record is now [5, -2, 4].
    *   - "C" - Invalidate and remove the previous score, record is now [5, -2].
    *   - "D" - Add 2 * -2 = -4 to the record, record is now [5, -2, -4].
    *   - "9" - Add 9 to the record, record is now [5, -2, -4, 9].
    *   - "+" - Add -4 + 9 = 5 to the record, record is now [5, -2, -4, 9, 5].
    *   - "+" - Add 9 + 5 = 14 to the record, record is now [5, -2, -4, 9, 5, 14].
    *   The total sum is 5 + -2 + -4 + 9 + 5 + 14 = 27.
    *
    * Example 3:
    *
    * - Input: ops = ["1","C"]
    * - Output: 0
    * - Explanation:
    *   - "1" - Add 1 to the record, record is now [1].
    *   - "C" - Invalidate and remove the previous score, record is now [].
    *   Since the record is empty, the total sum is 0.
    *
    * Constraints:
    *
    *   - 1 <= operations.length <= 1000
    *   - operations[i] is "C", "D", "+", or a string representing an integer in the range [-3 * 10^4, 3 * 10^4].
    *   - For operation "+", there will always be at least two previous scores on the record.
    *   - For operations "C" and "D", there will always be at least one previous score on the record.
    */

  def calPoints(operations: Array[String]): Int =
    val scores =
      operations
        .foldLeft(List.empty[Int]): (acc, element) =>
          element match
            case "C" => acc.tail
            case "D" => (acc.head * 2) +: acc
            case "+" => (acc.head + acc.tail.head) +: acc
            case n   => n.toInt +: acc
    // println(s"operations: [${operations.mkString(",")}] => [${scores.mkString(",")}]")
    scores.sum

/** 1275. Find winner on a tic tac toe game.
  *
  * Tic-tac-toe is played by two players A and B on a 3 x 3 grid. The rules of Tic-Tac-Toe are:
  *
  *    Players take turns placing characters into empty squares ' '.
  *    The first player A always places 'X' characters, while the second player B always places 'O' characters.
  *    'X' and 'O' characters are always placed into empty squares, never on filled ones.
  *    The game ends when there are three of the same (non-empty) character filling any row, column, or diagonal.
  *    The game also ends if all squares are non-empty.
  *    No more moves can be played if the game is over.
  *
  * Given a 2D integer array moves where moves[i] = [rowi, coli] indicates that the ith move will be played on grid[rowi][coli]. return the winner of the game if it exists (A or B). In case the game ends in a draw return "Draw". If there are still movements to play return "Pending".
  *
  * You can assume that moves is valid (i.e., it follows the rules of Tic-Tac-Toe), the grid is initially empty, and A will play first.
  *
  * Example 1
  *
  * | X |   |   |
  * |   | X |   |
  * | 0 | 0 | X |
  * - Input: moves = [[0,0],[2,0],[1,1],[2,1],[2,2]]
  * - Output: "A"
  * - Explanation: A wins, they always play first.
  *
  * ### Approach
  * - There are 8 lines to be considered; 3 row, 3 col and 2 diagonals.
  * - Place the moves and parse to find the player where one of the lines is completely occupied.
  */
def tictactoe(moves: Array[Array[Int]]): String =
  val arr = Array.ofDim[String](3, 3) // O(n*n)
  val played = moves.length
  // Fill board O(n)
  for i <- 0 until played by 2 do {
    val r = moves(i)(0)
    val c = moves(i)(1)
    arr(r)(c) = "A"
  }
  for i <- 1 until played by 2 do {
    val r = moves(i)(0)
    val c = moves(i)(1)
    arr(r)(c) = "B"
  }

  def check: Option[String] =
    var winner: Option[String] = None
    // check row & col O(n)
    for i <- 0 to 2 do {
      if winner.isEmpty && arr(i)(0) == arr(i)(1) && arr(i)(1) == arr(i)(2) && arr(i)(2) != null
      then winner = Some(arr(i)(0))
      if winner.isEmpty && arr(0)(i) == arr(1)(i) && arr(1)(i) == arr(2)(i) && arr(2)(i) != null
      then winner = Some(arr(0)(i))
    }
    // check diagonal  O(1)
    if winner.isEmpty && arr(0)(0) == arr(1)(1) && arr(1)(1) == arr(2)(2) && arr(1)(1) != null then
      winner = Some(arr(1)(1))
    if winner.isEmpty && arr(0)(2) == arr(1)(1) && arr(1)(1) == arr(2)(0) && arr(1)(1) != null then
      winner = Some(arr(1)(1))

    winner

  check match
    case Some(v)             => v
    case None if played == 9 => "Draw"
    case _                   => "Pending"
