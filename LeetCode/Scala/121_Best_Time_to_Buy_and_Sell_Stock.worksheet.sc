object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        var buyPrice : Int = prices(0)
        var profit : Int = 0

        for(i <- 0 until prices.length){

            if(prices(i) < buyPrice){
                buyPrice = prices(i)
            }
            else{
                var currentProfit : Int = prices(i) - buyPrice
                profit = Math.max(currentProfit, profit)
            }
        }

        profit
    }
}

val prices = Array(7,1,5,3,6,4)

Solution.maxProfit(prices)


object Solution1 {
  def maxProfit(prices: Array[Int]): Int = {
    def calculateMaxProfit(prices: List[Int], minPrice: Int, maxProfit: Int): Int = prices match {
      case Nil => maxProfit
      case price :: tail =>
        val currentProfit = price - minPrice
        val updatedMinPrice = Math.min(minPrice, price)
        val updatedMaxProfit = Math.max(maxProfit, currentProfit)
        calculateMaxProfit(tail, updatedMinPrice, updatedMaxProfit)
    }
    
    calculateMaxProfit(prices.toList, Int.MaxValue, 0)
  }
}
