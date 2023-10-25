/*
Given an array of intervals where intervals[i] = [starti, endi], merge all overlapping intervals, and return an array of the non-overlapping intervals that cover all the intervals in the input.

Example 1:
Input: intervals = [[1,3],[2,6],[8,10],[15,18]]
Output: [[1,6],[8,10],[15,18]]
Explanation: Since intervals [1,3] and [2,6] overlap, merge them into [1,6].

Example 2:
Input: intervals = [[1,4],[4,5]]
Output: [[1,5]]
Explanation: Intervals [1,4] and [4,5] are considered overlapping.
*/

object mergeIntervals extends App {

  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {

    val sorted = intervals.sortBy(_.head)

    sorted.indices.drop(1).toArray.foldLeft(Array(sorted(0)))((acc, i) => {
      if (acc.last(1) >= sorted(i)(0))
        acc.dropRight(1) :+
          Array(
            math.min(acc.last(0), sorted(i-1)(0)),
            math.max(acc.last(1), sorted(i)(1))
          )
      else
        acc :+ sorted(i)
      }
    )
  }

  val intervals: Array[Array[Int]] = Array(Array(1,2), Array(3,4), Array(5,6), Array(1,10))


  merge(intervals).map(_.mkString("[",",","]")).foreach(println)


}
