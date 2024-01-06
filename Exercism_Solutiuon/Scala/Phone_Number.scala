object PhoneNumber {
  def clean(rawPhoneNumber: String): Option[String] = {
    val number = rawPhoneNumber.filter(_.isDigit)

    number match {
      case n if n.matches("[2-9]\\d{2}[2-9]\\d{6}") => Some(n)
      case n if n.startsWith("1") && n.length == 11 => Some(n.tail)
      case _ => None
    }
  }
}


// Community solutions

object PhoneNumber {
  private val validNum = "1?([2-9][0-9]{2}[2-9][0-9]{6})".r

  def clean(number:String):Option[String] = {
    number.filter(_.isDigit) match {
      case validNum(number) => Some(number)
      case _ => None
    }
  }
}
