/*
Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]] such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.
Notice that the solution set must not contain duplicate triplets.

Example 1:
Input: nums = [-1,0,1,2,-1,-4]
Output: [[-1,-1,2],[-1,0,1]]
Explanation:
nums[0] + nums[1] + nums[2] = (-1) + 0 + 1 = 0.
nums[1] + nums[2] + nums[4] = 0 + 1 + (-1) = 0.
nums[0] + nums[3] + nums[4] = (-1) + 2 + (-1) = 0.
The distinct triplets are [-1,0,1] and [-1,-1,2].
Notice that the order of the output and the order of the triplets does not matter.

Example 2:
Input: nums = [0,1,1]
Output: []
Explanation: The only possible triplet does not sum up to 0.

Example 3:
Input: nums = [0,0,0]
Output: [[0,0,0]]
Explanation: The only possible triplet sums up to 0.
 */

object threeSum extends App {

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val zipped = nums.zipWithIndex
    val result =  scala.collection.mutable.ListBuffer.empty[List[Int]]
    for {
      (num1, i1) <- zipped
      (num2, i2) <- zipped.filter { case (_, i2) => i1 != i2 }
      _          <- zipped.map { case (num3, i3) =>
          if(0 == num1 + num2 + num3 && i1 != i3 && i2 != i3) result.addOne(List(num1, num2, num3)) }
    } yield ()

    result.toList.foldLeft(List.empty[List[Int]]) { case (acc, list) =>
      if (!acc.map(l => l.sorted).contains(list.sorted)) acc :+ list else acc
    }
  }

  def threeSum2(nums: Array[Int]): List[List[Int]] = {
    val map = nums.zipWithIndex.toMap
    (
      for {
        i1 <- LazyList.range(0, nums.length-2)
        i2 <- LazyList.range(i1 + 1, nums.length-1)
        num1 = nums(i1)
        num2 = nums(i2)
        num3 = -(num1 + num2)
        i3 = map.get(num3)
        if i3.isDefined && i3.get != i1 && i3.get != i2
        result = List(num1, num2, num3).sorted
    } yield result
      ).distinct.toList
  }

  val nums: Array[Int] = Array(-1, 0, 1, 2, -1, -4)

  println(
    threeSum2(nums).mkString("[",",","]")
  )

}
