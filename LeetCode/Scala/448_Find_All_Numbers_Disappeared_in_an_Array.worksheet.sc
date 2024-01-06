object Solution {
    def findDisappearedNumbers(nums: Array[Int]): List[Int] = {
        for (i <- nums){
            val index = Math.abs(i) - 1

            if(nums(index) > 0){
                nums(index) *= -1
            }

        }

        nums.zipWithIndex.collect({case (value, index) if value > 0 => index }).map(_+1).toList

    }
} 


// testing

var b = Array(4,3,2,7,8,2,3,1)

Solution.findDisappearedNumbers(b)

for (i <- b){
    val index = Math.abs(i) - 1

    if(b(index) > 0){
        b(index) *= -1
    }

    b
}

b.filter(_> 0)

b.zipWithIndex.collect({case (value, index) if value > 0 => index }).map(_+1).toList

b.zipWithIndex.mkString
val numbers: List[Int] = List(-2, 3, 0, 5, -1, 4, 0, 2)
val positions: List[Int] = numbers.zipWithIndex.collect { case (value, index) if value > 0 => index }

println(positions)



val a = List(-2,-5,3)

a.filter(_ > 0).map(_ + 1)