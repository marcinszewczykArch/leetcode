package BinarySearch

import scala.annotation.tailrec

/*
Given an array of integers nums which is sorted in ascending order, and an integer target, write a function to search target in nums. If target exists, then return its index. Otherwise, return -1.
You must write an algorithm with O(log n) runtime complexity.

Example 1:
Input: nums = [-1,0,3,5,9,12], target = 9
Output: 4
Explanation: 9 exists in nums and its index is 4

Example 2:
Input: nums = [-1,0,3,5,9,12], target = 2
Output: -1
Explanation: 2 does not exist in nums so return -1
*/

object search extends App {

  def search(nums: Array[Int], target: Int): Int = {

    @tailrec
    def run(subArray: Array[Int], firstIndex: Int): Int = subArray match {
      case Array(elem) => if (elem.equals(target)) firstIndex else -1
      case _ =>
        val middle = subArray.length / 2
        if (subArray(middle) > target) run(subArray.splitAt(middle)._1, firstIndex)
        else run(subArray.splitAt(middle)._2, firstIndex + middle)
    }

    run(nums, 0)
  }


  println(search(Array(-1, 0, 3, 5, 9, 12), 12))

}