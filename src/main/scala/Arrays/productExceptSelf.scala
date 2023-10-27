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

object productExceptSelf extends App {

  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    val productAll = nums.product
    nums.map {
      case n@0 => (nums diff List(n)).product
      case n   => productAll / n
    }

  }


  val nums: Array[Int] = Array(1,2,3,4)
  val target: Int = 9

  println(
    productExceptSelf(nums).mkString("[",",","]")
  )

}
