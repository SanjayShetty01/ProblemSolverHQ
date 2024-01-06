object Solution {
  def search(nums: Array[Int], target: Int): Int = {
    var left = 0
    var right = nums.length - 1

    while (left <= right) {
      val mid = left + (right - left) / 2

      if (nums(mid) == target) {
        return mid // Element found
      } else if (nums(mid) > target) {
        right = mid - 1 // Search in the left half
      } else {
        left = mid + 1 // Search in the right half
      }
    }

    -1 // Element not found
  }
}
