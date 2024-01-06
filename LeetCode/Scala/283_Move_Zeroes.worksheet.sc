// Two pointer approach

object Solution {
    def moveZeroes(nums: Array[Int]): Unit = {
        val size : Int = nums.length

        if(size == 0 || size == 1){
            return
        }

        var nz: Int = 0; var z :Int = 0

        while(nz < size){
            if(nums(nz) != 0){
                var temp: Int = nums(nz)
                nums(nz) = nums(z)
                nums(z) = temp
                nz += 1
                z += 1
            }

            else{
                nz += 1
            }
        }
    }
}