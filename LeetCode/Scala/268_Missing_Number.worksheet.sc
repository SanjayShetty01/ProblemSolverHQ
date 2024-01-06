object Solution {
    def missingNumber(nums: Array[Int]): Int = {
        val n = nums.length
        val exSum = (n * (n +1))/2
        val actSum = nums.sum

        exSum - actSum
    }
}


val nums : Array[Int] = Array(1,3,0)

Solution.missingNumber(nums)

object Solution2 {
  def missingNumber(nums: Array[Int]): Int = {
    val n = nums.length
    val expectedXOR = (0 to n).reduce(_ ^ _)
    val actualXOR = nums.reduce(_ ^ _)

    expectedXOR ^ actualXOR
  }
}

Solution2.missingNumber(nums)
