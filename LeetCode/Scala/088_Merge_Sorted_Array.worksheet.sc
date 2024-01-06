object Solution {
    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
        
        var m_var = m
        var n_var = n
        var k = m + n -1
        while(m_var > 0 && n_var > 0){
            if (nums1(m_var - 1) > nums2(n_var - 1)){
                nums1(k) = nums1(m_var -1)
                m_var -=1
            }
            else {
                nums1(k) = nums2(n_var -1)
                n_var -=1 
            }

            k -= 1
        }

        // filling nums2 with remaining num2 value
        while(n_var > 0){
            nums1(k) = nums2(n_var -1)
            n_var -= 1
            k -= 1
        }
    }
}


val nums1 = Array(1,2,3,0,0,0)
val nums2 = Array(2,5,6) 


Solution.merge(nums1, 3, nums2, 3)

// functional Apporach 
val g = Array(1,2,4,0,0,0)
val h = Array(2,5,8)
val m = 3
val n = 3

(g.take(m) ++ h.take(n)).sorted.mkString

object Solution1     {
    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
       (nums1.take(m) ++ nums2.take(n)).sorted.copyToArray(nums1)
    }
}

Solution1.merge(nums1, 3, nums2, 3)

