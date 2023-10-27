package Arrays

/*
Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
You may assume that each input would have exactly one solution, and you may not use the same element twice.
You can return the answer in any order.

Example 1:
Input: nums = [2,7,11,15], target = 9
Output: [0,1]
Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].

Example 2:
Input: nums = [3,2,4], target = 6
Output: [1,2]

Example 3:
Input: nums = [3,3], target = 6
Output: [0,1]
*/

object twoSum extends App {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val map = nums.zipWithIndex.toMap
    val firstIndex  = nums.indices.indexWhere(i => map.get(target - nums(i)).exists(_ != i))
    val secondIndex = map.find { case (number, index) => number == target - nums(firstIndex) && index != firstIndex }.map(_._2).getOrElse(-1)
    Array(firstIndex, secondIndex)
  }


  val nums: Array[Int] = Array(2,7,11,15)
  val target: Int = 9

  println(
    twoSum(nums, target).mkString("[",",","]")
  )

}
