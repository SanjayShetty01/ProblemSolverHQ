object Solution {
    def isPalindrome(x: Int): Boolean = {
        
        var rev = 0
        var temp = x

        while(temp > 0){
            var digit = temp%10
            rev = (rev *10)+ digit

            temp /=10 
        }

        x == rev
        
    }
}

Solution.isPalindrome(121)