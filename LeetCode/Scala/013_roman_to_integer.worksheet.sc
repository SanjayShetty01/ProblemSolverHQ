object Solution1 {

    private val romanToIntegerMap = Map(
        'I' -> 1,
        'V' -> 5,
        'X' -> 10,
        'L' -> 50,
        'C' -> 100,
        'D' -> 500,
        'M' -> 1000
    )
    def romanToInt(s: String): Int = {
        var result = 0
        var previousValue = 0

        for(i <- s.length - 1 to 0 by -1){
            val currentValue = romanToIntegerMap(s.charAt(i))

            if(currentValue >= previousValue){
                result += currentValue
            }
            else{
                result -= currentValue
            }

            previousValue = currentValue
        }

        return(result)
    }
}

Solution1.romanToInt("IX")


// Solutions from the internet

object Solution2 {
        def romanToInt(s: String): Int = {
      s.toCharArray.map(VALUE_MAP(_)).foldLeft((0, Int.MaxValue)) { (result, currentValue) => 
          //If we're decreasing or equal to the last value
          if (currentValue <= result._2) {
            (currentValue + result._1, currentValue)
          } else {
            //Wow we had an increase from the previous value lets double the last previous and subtract it from the current
            (result._1 - result._2*2 + currentValue, currentValue)
          }
        }._1
    }

    val VALUE_MAP= Map(
        'I'-> 1,
        'V' -> 5,
        'X' -> 10,
        'L' -> 50,
        'C' -> 100,
        'D' -> 500,
        'M' -> 1000
    )
}


Solution2.romanToInt("III")
