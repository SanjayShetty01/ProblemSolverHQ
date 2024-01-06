object HighScores{
  def latest(Scores: List[Int]) : Int = Scores.last
  
  def personalBest(Scores: List[Int]): Int = Scores.max

  def personalTop(Scores: List[Int]) : List[Int] = Scores.sorted.reverse.take(3)
  
  def report(Scores: List[Int]): String = {
    val latestScore: Int = latest(Scores)
    val differenceToLatest : Int = personalBest(Scores) - latestScore

    if (differenceToLatest > 0) {
    s"Your latest score was $latestScore. That's $differenceToLatest short of your personal best!"
    } 
  else {
    s"Your latest score was $latestScore. That's your personal best!"
  }
  } 
}