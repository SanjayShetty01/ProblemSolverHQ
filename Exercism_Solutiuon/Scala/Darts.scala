object Darts {
  def score(x : Double, y : Double) : Int = { math.hypot(x,y) match {
    case hypot if hypot > 10 => 0
    case hypot if hypot > 5 => 1
    case hypot if hypot > 1 => 5
    case _ => 10
  }
}
}