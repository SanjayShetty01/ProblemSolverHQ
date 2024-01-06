object FlattenArray {
  def flatten(lists: List[_]): List[Int] = {
    lists.flatMap {
      case list: List[_] => flatten(list)
      case list:Int => List(list)
      case null => List()
    }
  }
}