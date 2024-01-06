object Solution {
    def searchInsert(nums: Array[Int], target: Int): Int = {
        
        var low : Int = 0
        var high : Int = nums.length - 1

        while(low <= high){
            
            val mid: Int = low + (high - low) / 2

            if(nums(mid) == target){
                return(mid)
            }
            else if(nums(mid)  > target){
                low = mid - 1
            }
            else{
                low = mid + 1
            }
        }

        low
    }
}

object Solution1 {
  def searchInsert(nums: Array[Int], target: Int): Int = {
    var low: Int = 0
    var high: Int = nums.length - 1

    while (low <= high) {
      val mid: Int = low + (high - low) / 2

      if (nums(mid) == target) {
        return mid
      } else if (nums(mid) > target) {
        high = mid - 1
      } else {
        low = mid + 1
      }
    }

    low
  }
}




val nums = Array(1,3,5,6)

val target = 5
Solution.searchInsert(nums, 5)


/*         var low : Int = nums(0)
        var high : Int = nums(nums.length - 1)
        var mid : Int = (high/2).toInt

        while(low < high){
            if(nums(mid) == target){
                 mid
            }
            else if(nums(mid) > target){
                low = mid - 1
            }
            else{
                low = mid + 1
            }
        }
         */