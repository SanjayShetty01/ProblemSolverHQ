val strs: Array[String] = Array("flower", "flow", "flights")


object Solution1 {
        def longestCommonPrefix(strs: Array[String]): String = {
            val arraySorted : Array[String] = strs.sorted
            val firstElement : String = arraySorted.head
            val lastElement : String = arraySorted.last
            val n :Int = firstElement.length.min(lastElement.length)
            var answer: String = ""
            var i: Int = 0
            var done: Boolean = false

            while(i < n && !done){
                if(firstElement(i) != lastElement(i)){
                    done = true
                    answer
                }
                else{
                    answer += firstElement(i)
                }  
            
                i+= 1
            }

            return(answer)
        }
}

Solution2.longestCommonPrefix(strs)

object Solution2 {
    def longestCommonPrefix(strs: Array[String]): String = {

        val shortestWord: String = strs.minBy(_.length)
        var commonPrefix: String = shortestWord

        for (word <- strs) {
            while (!word.startsWith(commonPrefix)) {
                commonPrefix = commonPrefix.init
            }
        }

    return(commonPrefix)

    }
}

Solution1.longestCommonPrefix(strs)





