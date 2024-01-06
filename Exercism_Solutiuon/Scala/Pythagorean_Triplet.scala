import scala.math.pow


object PythagoreanTriplet{
  def isSeq(a : Int, b : Int, c : Int) : Boolean = if(a < b & b < c) true else false;

  def sumMatches(a : Int, b : Int, c : Int) : Boolean = if(pow(a,2) + pow(b,2) == pow(c,2)) true else false; 
  
  def isPythagorean(tuple : (Int,Int,Int)) : Boolean = {
    val (a,b,c) = tuple
    if(isSeq(a, b, c) && sumMatches(a, b, c)) true else false; 
   }

  def pythagoreanTriplets(start : Int, end : Int) : Seq[(Int, Int, Int)] = {
        for {
      a <- start to end
      b <- a to end
      c = Math.sqrt(a * a + b * b).toInt
      if isPythagorean((a, b, c)) && c <= end
    } yield (a, b, c)  
  }

  def pythagoreanTripletsSum(value : Int) : Seq[(Int, Int, Int)] = {
        pythagoreanTriplets(1, value).filter { case (a, b, c) => a + b + c == value }
  }
}

