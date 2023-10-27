package Arrays

/*
Given an array nums of size n, return the majority element.
The majority element is the element that appears more than ⌊n / 2⌋ times. You may assume that the majority element always exists in the array.

Example 1:
Input: nums = [3,2,3]
Output: 3

Example 2:
Input: nums = [2,2,1,1,1,2,2]
Output: 2
*/

object majorityElement extends App {

  def majorityElement(nums: Array[Int]): Int = {
//    val map: Map[Int, Array[Int]] = nums.groupBy(n => nums.count(_ == n))
//    map(map.keySet.max).head

//    nums.groupBy(identity).map { case (k, ints) => (k, ints.length) }.toList.maxBy(_._2)._1

//    nums.distinct.find(n => nums.toList.count(_ == n) > nums.length / 2).get

    nums.sorted.apply(nums.length/2)
  }


  val nums: Array[Int] = Array(2,2,1,1,1,2,2)

  println(
    majorityElement(nums)
  )

}
