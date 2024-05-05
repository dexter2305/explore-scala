package leetcode.explore

object ArrayAndString:

  /** Find pivot index
    *
    *     *Given an array of integers nums, calculate the pivot index of this array.
    *
    * The pivot index is the index where the sum of all the numbers strictly to the left of the index is equal to the sum of all the numbers strictly to the index's right.
    *
    * If the index is on the left edge of the array, then the left sum is 0 because there are no elements to the left. This also applies to the right edge of the array.
    *
    *     Return the leftmost pivot index. If no such index exists, return -1.
    *
    * Example 1:
    *
    * Input: nums = [1,7,3,6,5,6]
    * Output: 3
    * Explanation:
    * The pivot index is 3.
    * Left sum = nums[0] + nums[1] + nums[2] = 1 + 7 + 3 = 11
    * Right sum = nums[4] + nums[5] = 5 + 6 = 11
    *
    * Example 2:
    *
    * Input: nums = [1,2,3]
    * Output: -1
    * Explanation:
    * There is no index that satisfies the conditions in the problem statement.
    *
    * Example 3:
    *
    * Input: nums = [2,1,-1]
    * Output: 0
    * Explanation:
    * The pivot index is 0.
    * Left sum = 0 (no elements to the left of index 0)
    * Right sum = nums[1] + nums[2] = 1 + -1 = 0
    *
    * Constraints:
    *
    *    1 <= nums.length <= 104
    *    -1000 <= nums[i] <= 1000
    *
    * ### Approach
    * - get left sum array (l).
    * - get right sum array (r).
    * - compare to find if there is an index where l(i) == r(i),
    */
  def pivotIndex(nums: Array[Int]): Int =
    val leftSum = nums.scanLeft(0)(_ + _).take(nums.length)
    val rightSum = nums.scanRight(0)(_ + _).tail
    val search =
      leftSum
        .zip(rightSum)
        .zipWithIndex
        .find: pairWithIndex =>
          pairWithIndex match
            case ((x, y), i) => x == y
    search match
      case None           => -1
      case Some(_, index) => index

    /** Largest number at least twice of others.
      *
      * You are given an integer array nums where the largest integer is unique.
      *
      * Determine whether the largest element in the array is at least twice as much as every other number in the array. If it is, return the index of the largest element, or return -1 otherwise.
      *
      * Example 1:
      *
      * Input: nums = [3,6,1,0]
      * Output: 1
      * Explanation: 6 is the largest integer.
      * For every other number in the array x, 6 is at least twice as big as x.
      * The index of value 6 is 1, so we return 1.
      *
      * Example 2:
      *
      * Input: nums = [1,2,3,4]
      * Output: -1
      * Explanation: 4 is less than twice the value of 3, so we return -1.
      *
      * Constraints:
      *
      *    2 <= nums.length <= 50
      *    0 <= nums[i] <= 100
      *    The largest element in nums is unique.
      */
  def dominantIndex(nums: Array[Int]): Int = {
    val max = nums.max
    if nums.filter(_ != max).exists(_ * 2 > max) then -1 else nums.indexOf(max)
  }

  /** Plus one.
    *
    * You are given a large integer represented as an integer array digits, where each digits[i] is the ith digit of the integer. The digits are ordered from most significant to least significant in left-to-right order. The large integer does not contain any leading 0's.
    *
    * Increment the large integer by one and return the resulting array of digits.
    *
    * Example 1:
    *
    * Input: digits = [1,2,3]
    * Output: [1,2,4]
    * Explanation: The array represents the integer 123.
    * Incrementing by one gives 123 + 1 = 124.
    * Thus, the result should be [1,2,4].
    *
    * Example 2:
    *
    * Input: digits = [4,3,2,1]
    * Output: [4,3,2,2]
    * Explanation: The array represents the integer 4321.
    * Incrementing by one gives 4321 + 1 = 4322.
    * Thus, the result should be [4,3,2,2].
    *
    * Example 3:
    *
    * Input: digits = [9]
    * Output: [1,0]
    * Explanation: The array represents the integer 9.
    * Incrementing by one gives 9 + 1 = 10.
    * Thus, the result should be [1,0].
    *
    * Constraints:
    *
    * - 1 <= digits.length <= 100
    * - 0 <= digits[i] <= 9
    * digits does not contain any leading 0's
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
