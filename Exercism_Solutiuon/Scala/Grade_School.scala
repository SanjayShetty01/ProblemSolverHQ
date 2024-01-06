class School {
  type DB = Map[Int, Seq[String]]
  var database: DB = Map()

  def add(name: String, g: Int) = {
    // try appending the the name onto the dictionary including the grade
    database = database ++ Map(g -> (grade(g) :+ name))
  }

  def db: DB = database

  def grade(g: Int): Seq[String] = 
    // Get the names of students
    db.getOrElse(g, Seq())

  def sorted: DB = {
    // sort the database
    val sorted = database.map(x => (x._1, x._2.sorted))
    Map(sorted.toSeq.sortBy(_._1):_*)
}
}
