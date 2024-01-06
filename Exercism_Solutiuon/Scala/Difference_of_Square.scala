object DifferenceOfSquares {
  def square(x: Int): Int = x * x

  def sumOfSquares(n: Int): Int = {
    return(Seq.range(1,n+1,1).map(square).sum)
  }

  def squareOfSum(n: Int): Int = {
    return(square(Seq.range(1,n+1,1).sum))
  }

  def differenceOfSquares(n: Int): Int = {
    return(squareOfSum(n)- sumOfSquares(n))
  }
}
