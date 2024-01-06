object Solution {
    def removeDuplicates(nums: Array[Int]): Int = {
        var rd : Int = 0

        for(i <- 1 until nums.length){
            if(nums(rd) != nums(i)){
                rd += 1
                nums(rd) = nums(i)
            }
        }

        return(rd+1)
    }
}


var nums = Array(1,1,2)


Solution.removeDuplicates(nums)