// functional apporach

object Solution {
  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val set1 = nums1.toSet
    val set2 = nums2.toSet
    
    set1.intersect(set2).toArray
  }
}


