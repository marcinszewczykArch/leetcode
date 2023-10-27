package Arrays

/*
Given an array of distinct integers candidates and a target integer target, return a list of all unique combinations of candidates where the chosen numbers sum to target. You may return the combinations in any order.
The same number may be chosen from candidates an unlimited number of times. Two combinations are unique if the
frequency of at least one of the chosen numbers is different.
The test cases are generated such that the number of unique combinations that sum up to target is less than 150 combinations for the given input.

Example 1:
Input: candidates = [2,3,6,7], target = 7
Output: [[2,2,3],[7]]
Explanation:
2 and 3 are candidates, and 2 + 2 + 3 = 7. Note that 2 can be used multiple times.
7 is a candidate, and 7 = 7.
These are the only two combinations.

Example 2:
Input: candidates = [2,3,5], target = 8
Output: [[2,2,2,2],[2,3,3],[3,5]]

Example 3:
Input: candidates = [2], target = 1
Output: []
*/

object combinationSum extends App {

  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    val numbs = (0 +: candidates).sorted
    def range(rest: Int) = LazyList.range(0, numbs.count(_ <= rest))

    (for {
      i1 <- range(target)
      rest2 = target - numbs(i1)
      i2 <- range(rest2)
      rest3 = rest2 - numbs(i2)
      i3 <- range(rest3)
      rest4 = rest3 - numbs(i3)
      i4 <- range(rest4)
      rest5 = rest4 - numbs(i4)
      i5 <- range(rest5)
      rest6 = rest5 - numbs(i5)
      i6 <- range(rest6)
      rest7 = rest6 - numbs(i6)
      i7 <- range(rest7)
      rest8 = rest7 - numbs(i7)
      i8 <- range(rest8)
      rest9 = rest7 - numbs(i8)
      i9 <- range(rest9)
      rest10 = rest8 - numbs(i9)
      i10 <- range(rest10)


      result = List(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)
        .map(numbs(_))
        .filter(_ != 0)
        .sorted
      if result.sum == target

    } yield result).distinct.toList

  }

  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = go(candidates, target, Nil)

  def go(candidates: Array[Int], remaining: Int, acc: List[Int]): List[List[Int]] = {
    if (remaining < 0)
      Nil
    else if (remaining > 0 && candidates.isEmpty)
      Nil
    else if (remaining == 0 && candidates.isEmpty)
      List(acc)
    else
      go(candidates.tail, remaining, acc) :::
        go(candidates, remaining - candidates.head, candidates.head :: acc)
  }






  val candidates: Array[Int] = Array(4,8,11,10,9,3,12,7,6)
  val target: Int = 25

  println(
    combinationSum(candidates, target).mkString("[",",","]")
  )


}
