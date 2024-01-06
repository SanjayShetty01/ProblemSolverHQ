val nums = Array(1,2,1,3)

object Solution {
    def containsDuplicate(nums: Array[Int]): Boolean = {

        val uniqueHolder = scala.collection.mutable.HashSet[Int]()    
        
        for(num <- nums){

            if(uniqueHolder.contains(num)){
                return(true)
            }

            uniqueHolder.add(num)
        }
        false
    }
}

object Solution1 {
    def containsDuplicate(nums: Array[Int]): Boolean = {
        nums.toSet.size != nums.length
    }
}

object Solution3 {
    def containsDuplicate(nums: Array[Int]): Boolean = {
        nums.distinct.length != nums.length
    }
}



Solution.containsDuplicate(nums)

Solution1.containsDuplicate(nums)

Solution3.containsDuplicate(nums)
