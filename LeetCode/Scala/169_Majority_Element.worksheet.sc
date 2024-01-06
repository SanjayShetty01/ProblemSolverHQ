val nums = Array(2,2,1,3,1,2,2)


// Sorting method
object Solution {
    def majorityElement(nums: Array[Int]): Int = {
        nums.sorted(Ordering[Int])(nums.length/2)
    }
}

Solution.majorityElement(nums)
// using hashmap method
// groupBy will create Map
object Solution1 {
    def majorityElement(nums: Array[Int]): Int = {
        nums.groupBy(identity).maxBy(_._2.length)._1
    }
}

Solution1.majorityElement(nums)

// using Moore's Algo

object Solution3 {
    def majorityElement(nums: Array[Int]): Int = {
        var majority : Int = nums(0)
        var votes : Int = 1

        for(i <- 1 until nums.length){
            if(votes == 0){
                majority = nums(i)
                votes = 1
            }
            else if(majority == nums(i)){
                votes += 1
            }

            else{
                votes -= 1
            }

            
        }

        return(majority)
    }
}

Solution3.majorityElement(nums)
