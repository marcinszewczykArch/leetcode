package BinarySearch

import scala.annotation.tailrec

/*
You are a product manager and currently leading a team to develop a new product. Unfortunately, the latest version of your product fails the quality check. Since each version is developed based on the previous version, all the versions after a bad version are also bad.
Suppose you have n versions [1, 2, ..., n] and you want to find out the first bad one, which causes all the following ones to be bad.
You are given an API bool isBadVersion(version) which returns whether version is bad. Implement a function to find the first bad version. You should minimize the number of calls to the API.

Example 1:
Input: n = 5, bad = 4
Output: 4
Explanation:
call isBadVersion(3) -> false
call isBadVersion(5) -> true
call isBadVersion(4) -> true
Then 4 is the first bad version.

Example 2:
Input: n = 1, bad = 1
Output: 1
*/

object firstBadVersion extends App {
  def isBadVersion(version: Int): Boolean = version >= expectedFirstBadVersion

  def firstBadVersion(n: Int): Int = go(1, n)

  private def go(from: Int, to: Int): Int = {
    if (to - from < 2 && isBadVersion(from)) return from
    if (to - from < 2 && isBadVersion(to)) return to

    val half: Int = (BigInt(from) + BigInt(to))./(2).toInt

    println("from: " + from + "  to: " + to + "  half: " + half + "  isBadVersionHalf: " + isBadVersion(half))

    if (isBadVersion(half)) go(from, half) else go(half, to)
  }

  val max = Int.MaxValue
  val expectedFirstBadVersion = 1702766719

  println(firstBadVersion(max))
}