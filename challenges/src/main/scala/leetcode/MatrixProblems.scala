package leetcode

/** Matrix problems */
object MatrixProblems:

  /** 1572. Matrix diagonal sum.
    *
    * Given a square matrix mat, return the sum of the matrix diagonals.
    *
    * Only include the sum of all the elements on the primary diagonal and all the elements on the secondary diagonal that are not part of the primary diagonal.
    *
    * Example 1:
    *
    * - Input: mat = [[1,2,3],
    *                 [4,5,6],
    *                 [7,8,9]]
    * - Output: 25
    * - Explanation: Diagonals sum: 1 + 5 + 9 + 3 + 7 = 25
    * Notice that element mat[1][1] = 5 is counted only once.
    *
    * Example 2:
    *
    * - Input: mat = [[1,1,1,1],
    *                 [1,1,1,1],
    *                 [1,1,1,1],
    *                 [1,1,1,1]]
    * - Output: 8
    *
    * Example 3:
    *
    * - Input: mat = [[5]]
    * - Output: 5
    *
    * Constraints:
    *
    *  - n == mat.length == mat[i].length
    *  - 1 <= n <= 100
    *  - 1 <= mat[i][j] <= 100
    *
    * ### Approach
    *
    * - We dont need a nested for loop. Every iteration two elements will get summed up. Each of those elements belong to two diagonals.
    * - Elements from first diagonal come from (i, i), where i is row number.
    * - Second diagonal is tricky. Matrix is to determine the formula for second diagonal elements.
    *
    * | *(0,0)* | (0,1)   | *(0,2)* | (0, n-1)
    * | (1,0)   | *(1,1)* | (1,2)   | (1, (n-1) -1)
    * | *(2,0)* | (2,1)   | *(2,2)* | (2, (n-1) -2)
    *
    * So, the generic formula would be `(i, n-1-i)` where i is row and n is the total column count.
    * - Finally if the diagonals intersect at a cell, that cell will get counted twice. This happens when column count is odd. Adjust sum accordingly.
    */

  def diagonalSum(mat: Array[Array[Int]]): Int =
    val gt =
      (for i <- mat.indices yield mat(i)(i) + mat(i)(mat.length - i - 1)).sum
    if mat.length % 2 == 0 then gt else gt - mat(mat.length / 2)(mat.length / 2)

  /** 54. Spiral Matrix.
    *
    * Given an m x n matrix, return all elements of the matrix in spiral order.
    *
    * Example 1
    *   - Input: matrix = [[1,2,3],[4,5,6],[7,8,9]]
    *   - Output: [1,2,3,6,9,8,7,4,5]
    *
    * Constraints:
    *
    *    - m == matrix.length
    *    - n == matrix[i].length
    *    - 1 <= m, n <= 10
    *    - -100 <= matrix[i][j] <= 100
    */
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    def goRight(matrix: Array[Array[Int]]): List[Int] = {
      if matrix.nonEmpty && matrix(0).nonEmpty then {
        val upLine = matrix(0)
        val newMatrix = matrix.drop(1)

        upLine.toList ++ goDown(newMatrix)
      } else List.empty
    }

    def goDown(matrix: Array[Array[Int]]): List[Int] = {
      if matrix.nonEmpty && matrix(0).nonEmpty then {

        val downLine = matrix.map(row => row(row.length - 1))
        val newMatrix = matrix.map(row => row.dropRight(1))

        downLine.toList ++ goLeft(newMatrix)
      } else List.empty

    }

    def goLeft(matrix: Array[Array[Int]]): List[Int] = {
      if matrix.nonEmpty && matrix(0).nonEmpty then {

        val leftLine = matrix(matrix.length - 1)
        val newMatrix = matrix.dropRight(1)

        leftLine.reverse.toList ++ goUp(newMatrix)
      } else List.empty

    }

    def goUp(matrix: Array[Array[Int]]): List[Int] = {
      if matrix.nonEmpty && matrix(0).nonEmpty then {
        val upLine = matrix.map(elem => elem(0))
        val newMatrix = matrix.map(elem => elem.drop(1))

        upLine.reverse.toList ++ goRight(newMatrix)
      } else List.empty
    }

    goRight(matrix = matrix)
  }

  /** 73. Set matrix zeroes.
    *
    * Given an m x n integer matrix matrix, if an element is 0, set its entire row and column to 0's.
    *
    * You must do it in place.
    *
    * Example:1
    *
    * Input: matrix =
    *  [[1,1,1],
    *   [1,0,1],
    *   [1,1,1]]
    * Output:
    *  [[1,0,1],
    *   [0,0,0],
    *   [1,0,1]]
    *
    * ### Approach
    * - Create row and col markers as List. Pre fill them with 1.
    * - Any element in the matrix identified as 0, mark the (r,c) with 0 in the markers.
    * - Iterate through the matrix and multiply the value with markers.
    */
  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    val rowIdentifiers = Array.fill(matrix.length)(1)
    val colIdentifiers = Array.fill(matrix(0).length)(1)
    for
      r <- matrix.indices
      c <- matrix(0).indices
    do
      if matrix(r)(c) == 0 then
        rowIdentifiers(r) = 0
        colIdentifiers(c) = 0

    // println(s"row = [${rowIdentifiers.mkString(",")}]")
    // println(s"col = [${colIdentifiers.mkString(",")}]")

    for
      r <- 0 until matrix.length
      c <- 0 until matrix(r).length
    do matrix(r)(c) = rowIdentifiers(r) * colIdentifiers(c) * matrix(r)(c)

    // val s = s"${matrix.map(_.mkString(" ")).mkString("\n")}"
    // println(s)
  }

  /** 498. Diagonal matrix.
    *
    * Given an m x n matrix mat, return an array of all the elements of the array in a diagonal order.
    * Example 1:
    *
    * Input: mat = [[1,2,3],[4,5,6],[7,8,9]]
    * Output: [1,2,4,7,5,3,6,8,9]
    *
    * Example 2:
    *
    * Input: mat = [[1,2],[3,4]]
    * Output: [1,2,3,4]
    *
    * Constraints:
    *
    *    m == mat.length
    *    n == mat[i].length
    *    1 <= m, n <= 10^4
    *    1 <= m * n <= 10^4
    *    -10^5 <= mat[i][j] <= 10^5
    *
    * ### Approach
    * - Collect the diagonals first.
    * - How to collect diagonals? Sum (x, y) indices are constant for all elements in a diagonal.
    * - In this problem, just reverse the diagonal where there index sum % 2 == 0
    */

  def findDiagonalOrder(matrix: Array[Array[Int]]): Array[Int] = {
    val indexSumMap: Seq[(Int, Int)] =
      for
        r <- matrix.indices
        c <- matrix(r).indices
      yield (r + c -> matrix(r)(c))

    val indexSumMapWithElements: Map[Int, Seq[Int]] =
      indexSumMap
        .groupBy:
          case (rcSum, v) => rcSum
        .map(t => (t._1 -> t._2.map(_._2)))

    val result: Array[Int] =
      indexSumMapWithElements
        .flatMap:
          case (index, elements) => if index % 2 == 0 then elements.reverse else elements
        .toArray
    result
  }
