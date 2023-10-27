package Arrays

/*
Given an array, find the average of all contiguous subarrays of size ‘K’ in it.

Example 1:
Input: nums = Array: [1, 3, 2, 6, -1, 4, 1, 8, 2], K=5
Output: [2.2, 2.8, 2.4, 3.6, 2.8]
*/

object slidingWindow extends App {

  //bruteforce O(n*k)
  def find_averages_of_subarrays_bruteforce(nums: Array[Int], k: Int): Array[Double] =
    nums.sliding(k)
      .map(_.sum.toDouble/k)
      .toArray

  //clever way O(n)
  def find_averages_of_subarrays(nums: Array[Int], k: Int): Array[Double] = {
    val firstWindow = nums.take(k)
    def nextWindow(previous: Array[Int], lastIndex: Int) = previous.drop(1).appended(nums.apply(lastIndex))

    (firstWindow +: nums.indices.drop(k).map(nextWindow(firstWindow, _)))
      .map(_.sum.toDouble/k)
      .toArray
  }

  def findMaxSumSubArray(nums: Array[Int], k: Int): Int = {
    val firstWindow = nums.take(k)
    val firstSum = firstWindow.sum

    def nextSum(previousSum: Int, lastIndex: Int): Int = {
      previousSum - nums.apply(lastIndex - k) + nums.apply(lastIndex)
    }

    nums.indices.drop(k).foldLeft(firstSum, firstSum) { case ((maxSum, previousSum), lastIndex) =>
      val newSum = nextSum(previousSum, lastIndex)
      if (newSum > maxSum) (newSum, newSum) else (maxSum, newSum)
    }._1
  }




  val nums: Array[Int] = Array(1, 3, 2, 6, -1, 4, 1, 8, 2)
  val numsBig = (1 to 100000).toArray
  val k: Int = 5

  println(findMaxSumSubArray(numsBig, 2))

//  val a1 = System.nanoTime()
//  find_averages_of_subarrays_bruteforce(numsBig, k)
//  println((System.nanoTime() - a1)/1000000)
//
//  val a2 = System.nanoTime()
//  find_averages_of_subarrays(numsBig, k)
//  println((System.nanoTime() - a2)/1000000)
//
//  println(
//    find_averages_of_subarrays(nums, k).mkString("Array(", ", ", ")")
//  )

}
