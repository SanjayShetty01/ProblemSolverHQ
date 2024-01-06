object BinarySearch {
  def find(list: List[Int], number: Int): Option[Int] = {
    if(list.contains(number)) Some(list.indexOf(number))
    else None
  }
}