object ArmstrongNumbers{

  def nDigits(x: Int) = {
          import math._
          ceil(log(abs(x)+1)/log(10)).toInt
      }

  def isArmstrongNumber(num : Int): Boolean = {
      val N = nDigits(num)
      return(num == (for (i <- num.toString) yield Math.pow(i.asDigit, N)).sum)
  }
}

