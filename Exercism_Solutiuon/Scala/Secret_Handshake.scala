object SecretHandshake {
  
  def commands(n: Int): List[String] = {
    
    def isValidCommand(command: String): Boolean =
      Set("wink", "double blink", "close your eyes", "jump").contains(command)

    val binary = n.toBinaryString.reverse
    val actions = List("wink", "double blink", "close your eyes", "jump")

    val handshake = binary.zipWithIndex.collect {
      case ('1', idx) if idx < 4 => actions(idx)
    }.toList 
   
    if (binary.length >= 5 && binary(4) == '1') {
      handshake.reverse.filter(isValidCommand)
    } else {
      handshake.filter(isValidCommand)
    }
  }
}



// community solution i liked

object SecretHandshake {
  def commands(x: Int) : List[String] = {
    if (x >=16) commands(x - 16).reverse
    else if (x >= 8) commands(x - 8):+ "jump"
    else if (x >= 4) commands(x - 4):+ "close your eyes"
    else if (x >= 2) commands(x - 2):+ "double blink"
    else if (x >= 1) commands(x - 1):+ "wink"
    else List()
  }
}


// improving using pattern matching
object SecretHandshake {
  def commands(x: Int): List[String] = {
    def handshake(x: Int, actions: List[String]): List[String] = x match {
      case 0 => actions
      case n if n >= 16 => handshake(n - 16, actions.reverse)
      case n if n >= 8 => handshake(n - 8, actions :+ "jump")
      case n if n >= 4 => handshake(n - 4, actions :+ "close your eyes")
      case n if n >= 2 => handshake(n - 2, actions :+ "double blink")
      case n if n >= 1 => handshake(n - 1, actions :+ "wink")
    }

    handshake(x, List())
  }
}

