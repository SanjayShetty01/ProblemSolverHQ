object Solution {
    def removeElement(nums: Array[Int], `val`: Int): Int = {
        var r = 0

        for(i <- 0 until nums.length){
            if( nums(i) != `val`){
                nums(r) = nums(i)

                r += 1
            }
        }

        return(r)
    }
}

val nums = Array(3,2,2,3) 
val `val` = 3


Solution.removeElement(nums, `val`)