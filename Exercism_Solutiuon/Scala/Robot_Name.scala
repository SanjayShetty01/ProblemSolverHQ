import scala.util.Random

object RandomNameGenerator {
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val numbers = "0123456789"
  val random = new Random()

  def generateRandomString(): String = {
    val randomLetters = (1 to 2).map(_ => alphabet(random.nextInt(alphabet.length)))
    val randomDigits = (1 to 3).map(_ => numbers(random.nextInt(numbers.length)))
    (randomLetters ++ randomDigits).mkString
  }
}


class Robot{ 
  var name = RandomNameGenerator.generateRandomString()

  def reset(){
    this.name = RandomNameGenerator.generateRandomString()
  }
}