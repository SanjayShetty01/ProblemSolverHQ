object Solution {
    def maximumProduct(nums: Array[Int]): Int = {
        val numsSorted = nums.sorted
        var maxProd = numsSorted.takeRight(3).product

        if(numsSorted.count(_< 0) > 1){
            val negativeNumProd = numsSorted.take(2).product * numsSorted.last
            maxProd = math.max(negativeNumProd, maxProd)
        }
        maxProd      
    }
}

object Solution2 {
    def maximumProduct(nums: Array[Int]): Int = {
        val numsSorted = nums.sorted
        val positiveNumProd = numsSorted.takeRight(3).product
        val negativeNumProd = numsSorted.take(2).product * numsSorted.last
        
        val maxProd = math.max(negativeNumProd, positiveNumProd)

        return(maxProd)      
    }
}
