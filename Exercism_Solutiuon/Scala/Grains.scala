object Grains {
  def square(n: Int) = {
    if(n > 0 & n < 65) Some(BigInt(1) << n -1) else None 
  }

  def total = {
    (BigInt(1) << 64) -1
  }
}