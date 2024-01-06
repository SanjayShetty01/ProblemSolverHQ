object NumberType extends Enumeration {
  type NumberType = Value
  val Deficient, Perfect, Abundant = Value
}

object PerfectNumbers{
  import NumberType._
  def aliquotSum(num : Int) : Int = {
    val nums = (1 until num).toList
    val aliquotList : List[Int] =  for {
      i <- nums
      if num % i == 0
    } yield i

    return((aliquotList).sum)
  }

  def classify(num : Int) : Either[String, NumberType] = {

    if(num < 1){
      return(Left("Classification is only possible for natural numbers.")) 
    }
  else{
    aliquotSum(num) match {
      case x if x > num => Right(NumberType.Abundant)
      case x if x == num => Right(NumberType.Perfect)
      case x if x < num => Right(NumberType.Deficient)
    }
  }
  }
}