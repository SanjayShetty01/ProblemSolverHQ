// practice
val s: String = "(]"

val brackets = Map(
        "["  -> "]",
        "(" -> ")",
        "{"  -> "}"
    )

brackets.keys.forall(_.contains(']'))

var temp : Array[String] = Array()

for (c <- s){
    if(brackets.keys.forall(_.contains(c))){
        if(temp.nonEmpty && temp.last.equals(brackets.get("["))){
            temp = temp.init
        }
    }
    else{
        temp = temp :+ c.toString
    }
}

if temp.isEmpty then true else false

val c = '{'

var temp1 : Array[String] = Array()

temp1 = temp1 :+ "{"

temp1.mkString

brackets.contains(c.toString)

temp1.nonEmpty && temp1.last.equals(brackets.get(c.toString).get)
//temp1 = temp1.init

c.toString

temp1.last

brackets.get(c.toString).get

temp1.mkString

temp1.takeRight(1).mkString



object Solution {

    private val brackets = Map(
        "]"  -> "[",
        ")" -> "(",
        "}"  -> "{"
    )
    
    def isValid(s: String): Boolean = {

        var temp : Array[String] = Array()

        if(s.length % 2 == 0){
        for (c <- s){
            if(brackets.contains(c.toString)){
                if(temp.nonEmpty && temp.last.equals(brackets.get(c.toString).get)){
                    temp = temp.init
                }
            }
            
            else{
                temp = temp :+ c.toString
            }
        
        }
            return(if temp.isEmpty then true else false)

    }
    return(false)
    
    }
} 

val s1: String = "{[]}"

s1.length % 2 == 0

var temp2 : Array[String] = Array()

temp2.isEmpty

Solution.isValid(s1)


object Solution2{
    private val brackets = Map(
        "]"  -> "[",
        ")" -> "(",
        "}"  -> "{"
    )

        def isValid(s: String): Boolean = {
            
            var temp: Array[String] = Array()
            
            for(c <- s){
                if(brackets.contains(c.toString)){
                    temp = temp :+ c.toString
                }else if(temp.nonEmpty && temp.last.equals(brackets.get(c.toString).get)){
                    temp = temp.init
                }
                else{
                    return(false) 
                }
            }
            
            return(temp.isEmpty)
        
        }
}



Solution2.isValid(s1)


import scala.collection.mutable.Stack


object Solution3{
    private val stack = Stack[Char]()
    private val openingParentheses = Set('(', '[', '{')
    private val closingParentheses = Set(')', ']', '}')
    private   val matchingPairs = Map( ']' -> '[' ,')' -> '(',  '}' -> '{')

    def isValid(s: String): Boolean = {
          for (c <- s) {
            if (openingParentheses.contains(c)) {
                  stack.push(c)
                } 
            else if (closingParentheses.contains(c)) {
                if (stack.isEmpty || stack.pop() != matchingPairs(c)) {
                    return false
                  }
                }
            }

        stack.isEmpty


     }

 
}


val s3: String = "{[]}"

Solution3.isValid(s3)

def isValidParentheses(s: String): Boolean = {
  val stack = Stack[Char]()
  val openingParentheses = Set('(', '[', '{')
  val closingParentheses = Set(')', ']', '}')
  val matchingPairs = Map( ']' -> '[' ,')' -> '(',  '}' -> '{')

  for (c <- s) {
    if (openingParentheses.contains(c)) {
      stack.push(c)
    } else if (closingParentheses.contains(c)) {
      if (stack.isEmpty || stack.pop() != matchingPairs(c)) {
        return false
      }
    }
  }

  stack.isEmpty
}

val input1 = "([]"
val input2 = "()[]{}"
val input3 = "(]"
val input4 = "([)]"
val input5 = "{[]}"

println(isValidParentheses(input1)) // Output: true
println(isValidParentheses(input2)) // Output: true
println(isValidParentheses(input3)) // Output: false
println(isValidParentheses(input4)) // Output: false
println(isValidParentheses(input5)) // Output: true
