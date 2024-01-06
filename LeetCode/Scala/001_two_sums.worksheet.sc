// Brute Force using indices

object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
                val indices = nums.indices
        for {i <- indices; j <- indices} {
            if (i != j && nums(i) + nums(j) == target) return Array(i, j)
        }

        Array()
    }
}


// Hash Map solution

object Solution1 {

def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val hashMap = scala.collection.mutable.Map[Int, Int]() 
    for (i <- 0 until nums.length) {
        val complement = target - nums(i)
        if (hashMap.contains(complement)) {
            return Array(hashMap(complement), i)
            // array would contain the position of first and second elements position
        }
        hashMap(nums(i)) = i
    }
    throw new IllegalArgumentException("No two sum solution")
}
}

