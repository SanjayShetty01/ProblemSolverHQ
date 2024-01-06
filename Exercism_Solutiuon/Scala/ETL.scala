object Etl {
  def transform(scoreMap: Map[Int, Seq[String]]): Map[String, Int] = {
   scoreMap.flatMap(x => x._2.map(y => (y.toLowerCase -> x._1))) 
  }
}
