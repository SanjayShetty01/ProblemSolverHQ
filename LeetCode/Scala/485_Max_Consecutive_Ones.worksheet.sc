object Solution {
    def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
        var maxCount : Int = 0
        var count : Int = 0

        for(i <- nums){
            if(i == 1){
                count += 1
            }
        
            else{
                if(count > maxCount){
                    maxCount = count
                }
            count = 0
            
            }
        
         if(maxCount < count){
            maxCount = count
         }
     
        }

        maxCount       
}
}


object Solution1 {
    def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
        var maxCount : Int = 0
        var count : Int = 0

        for(i <- nums){
            if(i == 1){
                count += 1
                maxCount = Math.max(count, maxCount)
            }
        
            else{
                count = 0
            }
     
        }

    maxCount       
}
}

// functional way

object Solution3 {
    def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
        val (_, maxCount) = nums.foldLeft((0,0)) {case ((count, maxCount), nums) =>
            if (nums == 1) (count + 1, Math.max(maxCount, count + 1))
            else(0, maxCount)
        }
        maxCount
    }
}


val nums = Array(1,0,1,1,0,1)

Solution1.findMaxConsecutiveOnes(nums)
Solution3.findMaxConsecutiveOnes(nums)
// Practise
var maxCount : Int = 0
var count : Int = 0


for(i <- nums){
    if(i == 1){
        count += 1
    }

    else{
        if(count > maxCount){
            maxCount = count
        }
    count = 0
    
    }

 if(maxCount < count){
    maxCount = count
 }

}

maxCount