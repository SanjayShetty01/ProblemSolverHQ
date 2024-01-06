object Solution {
    def plusOne(digits: Array[Int]): Array[Int] = {
        
        for(i <- digits.indices.reverse){
            if(digits(i) < 9){
                digits(i) += 1

                return(digits)
            }
            else{
                digits(i) = 0
            }
        }
        1 +: digits
    }   
}


val digits = Array(1,1)

Solution.plusOne(digits).mkString
