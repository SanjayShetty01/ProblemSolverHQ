case class Triangle(a :Double, b :Double, c :Double) {
  
  def correct: Boolean = a > 0 && b > 0 && c > 0 && (a + b > c) && (a + c > b) && (b + c > a)
  
  val sides = Seq(a, b, c)
  
  def equilateral : Boolean = {
    if(correct && sides.distinct.size == 1) true
    else false
  }
  
  def isosceles : Boolean = {
    if (correct && sides.distinct.size <= 2 ) true
    else false
  }
  
  def scalene : Boolean = {
    if (correct && sides.distinct.size == 3 ) true
    else false
  }
}
