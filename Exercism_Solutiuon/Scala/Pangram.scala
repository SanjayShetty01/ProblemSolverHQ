object Pangrams {
  def isPangram(input: String): Boolean = {
    val wordClean = input.replaceAll("[^a-zA-Z]", "").toLowerCase.distinct;
    if(wordClean.length < 26){
      return(false);
    }else{
    return(true);
    }
  }
}

