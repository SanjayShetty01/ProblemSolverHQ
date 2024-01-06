object Bob {

  def hasLetters(word: String): Boolean ={
    return(word.exists(_.isLetter))
  }

  def silence(word: String) : Boolean = {
    return(word.forall(_.isWhitespace))
  }

  def shouting(word: String) : Boolean = {
    return(word.toUpperCase().equals(word))
  }

  def question(word: String): Boolean = {
    val wordTrimed= word.trim()
    return(wordTrimed.takeRight(1) == "?")
  }

  
  def response(statement: String): String = {
    if(shouting(statement) && hasLetters(statement) && question(statement)){
      return("Calm down, I know what I'm doing!");
    } else if(question(statement)){
      return("Sure.");
    } else if(shouting(statement) && question(statement) || silence(statement)){
      return("Fine. Be that way!");
    }else if(shouting(statement) && hasLetters(statement)){
      return("Whoa, chill out!");
    } else {
      return("Whatever.");
    }
  }  
}
