object Isogram{
  def isIsogram(word : String): Boolean = {
   val wordClean = word.replaceAll("\\s", "").replaceAll("-", "").toLowerCase();
    wordClean == wordClean.distinct;
}
}