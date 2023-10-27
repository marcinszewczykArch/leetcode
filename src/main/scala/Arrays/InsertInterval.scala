package Arrays

/*
You are given an array of non-overlapping intervals intervals where intervals[i] = [starti, endi] represent the start and the end of the ith interval and intervals is sorted in ascending order by starti. You are also given an interval newInterval = [start, end] that represents the start and end of another interval.
Insert newInterval into intervals such that intervals is still sorted in ascending order by starti and intervals still does not have any overlapping intervals (merge overlapping intervals if necessary).
Return intervals after the insertion.

Example 1:
Input: intervals = [[1,3],[6,9]], newInterval = [2,5]
Output: [[1,5],[6,9]]

Example 2:
Input: intervals = [[1,2],[3,5],[6,7],[8,10],[12,16]], newInterval = [4,8]
Output: [[1,2],[3,10],[12,16]]
Explanation: Because the new interval [4,8] overlaps with [3,5],[6,7],[8,10].
*/

object InsertInterval extends App {

  def insert(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    //intervals is empty
    if(intervals.isEmpty) return Array(newInterval)

    //new interval before
    if(newInterval(1) < intervals(0)(0)) return newInterval +: intervals

    //new interval after
    if(newInterval(0) > intervals.last(1)) return intervals :+ newInterval

    //intervals with 1 element
    if (intervals.length == 1) {
      val oldInterval = intervals(0)
      val a1 = oldInterval(0)
      val b1 = oldInterval(1)
      val a2 = newInterval(0)
      val b2 = newInterval(1)
      if(a1 <= a2 && b1 >= b2)  return intervals
      if(a2 <= a1 && b2 >= b1)  return Array(newInterval)
      if(a1 <= a2 && b2 > b1 && b1 >= a2)  return Array(Array(a1, b2))
      if(a2 <= a1 && b2 <= b1 && a1 <= b2)  return Array(Array(a2, b1))
      if(a1 <= a2 && b2 >= b1 && b1 < a2) return intervals :+ newInterval
      if(a2 <= a1 && b2 <= b1 && b2 < b1) return newInterval +: intervals
    }

    //intervals with >1 elements
    val firstSplit = intervals.indexWhere(_(1) >= newInterval(0))
    val lastSplit = intervals.indexWhere(_(0) > newInterval(1))

    val before = if(firstSplit == -1) Array.empty[Array[Int]] else intervals.splitAt(firstSplit)._1
    val after =  if(lastSplit == -1)  Array.empty[Array[Int]] else intervals.splitAt(lastSplit)._2
    val inside = intervals.dropRight(after.size).drop(before.size)


    println("before:")
    before.foreach(i => print(i.mkString("[", ",", "]")))
    println("")
    println("inside:")
    inside.foreach(i => print(i.mkString("[", ",", "]")))
    println("")
    println("after:")
    after.foreach(i => print(i.mkString("[", ",", "]")))
    println("")

    val intervalToAdd = Array(
      Math.min(newInterval(0), scala.util.Try(inside(0)(0)).toOption.getOrElse(newInterval(0))),
      Math.max(newInterval(1), inside.lastOption.map(_(1)).getOrElse(newInterval(1)))
    )

    before ++ Array(intervalToAdd) ++ after
  }

  def insert2(intervals: Array[Array[Int]], newInterval: Array[Int]): Array[Array[Int]] = {
    val (begin, next) = intervals.span(_(1) < newInterval(0))
    val (mid, end) = next.span(_(0) <= newInterval(1))
    val im = mid.fold(newInterval) { case (Array(a1, a2), Array(i1, i2)) => Array(a1 min i1, a2 max i2) }
    begin ++ Array(im) ++ end
  }

  val intervals = Array(Array(1,2), Array(3,5), Array(6,7), Array(8,10), Array(12,16))
  val newInterval = Array(4,8)

  insert2(intervals, newInterval).foreach(i => print(i.mkString("[", ",", "]")))


}
