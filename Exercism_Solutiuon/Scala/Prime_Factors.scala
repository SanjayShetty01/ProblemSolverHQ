object PrimeFactors {
  def factors(number: Long): List[Long] = {
    var n = number
    var divisor = 2
    val factors = collection.mutable.ListBuffer[Long]()

    while (n > 1 && divisor <= math.sqrt(number)) {
      while (n % divisor == 0) {
        factors += divisor
        n /= divisor
      }
      divisor += 1
    }

    if (n != 1)
      factors += n

    factors.toList
  }
}



// Solution from the community

import scala.annotation.tailrec

object PrimeFactors{
  
def factors(num: Long): List[Long] = {

  @tailrec
  def factorsRec(n: Long, i: Long, acc: List[Long]): List[Long] = {
    if (n == 1) {
      acc
    } else if (n % i == 0) {
      factorsRec(n / i, i, acc :+ i)
    } else {
      factorsRec(n, i + 1, acc)
    }  
  }

  factorsRec(num, 2, List.empty[Long])
}

  
}

object PrimeFactors {

  def factors(input: Long): List[Long] = {

    def factorsRec(start: Long, input: Long): List[Long] = {
      input match {
        case x if x == 1 => List()
        case _ if input % start == 0 => start :: factorsRec(start, input / start)
        case _ if input % start != 0 => factorsRec(start + 1, input)
      }
    }
    factorsRec(2, input)
  }
}