package String

import scala.util.Try
import scala.util.control.Breaks.break

/*
Given two strings s and t of lengths m and n respectively, return the minimum window
substringbof s such that every character in t (including duplicates) is included in the window. If there is no such substring, return the empty string "".

Example 1:
Input: s = "ADOBECODEBANC", t = "ABC"
Output: "BANC"
Explanation: The minimum window substring "BANC" includes 'A', 'B', and 'C' from string t.

Example 2:
Input: s = "a", t = "a"
Output: "a"
Explanation: The entire string s is the minimum window.

Example 3:
Input: s = "a", t = "aa"
Output: ""
Explanation: Both 'a's from t must be included in the window.
Since the largest window of s only has one 'a', return empty string.
*/

object minWindow extends App {

  def minWindow(s: String, t: String): String = {
    if(((s diff t) + t).length != s.length) return "" //string does not contain target

    val sLength = s.length
    var result = s
    var pointerR = 0
    var pointerL = 0
    val targetMap = collection.mutable.Map() ++ t.groupBy(identity).mapValues(_.length)

    def isWindowContainingTarget: Boolean = !targetMap.values.exists(_ > 0)
    def updateResult(): Unit = {
      val substring = s.drop(pointerL).dropRight(sLength - pointerR)
      if (result.length > substring.length) result = substring
    }
    def addRemovedCharToTargetMap(char: Char): Unit = if (targetMap.contains(char)) targetMap(char) = targetMap(char) + 1
    def removeAddedCharToTargetMap(char: Char): Unit = if (targetMap.contains(char)) targetMap(char) = targetMap(char) - 1

    while(pointerR <= sLength) {
      if (isWindowContainingTarget) {
        updateResult()
        addRemovedCharToTargetMap(s(pointerL))
        pointerL = pointerL + 1 //narrow the window on the left
      } else if (pointerR == sLength) {
        pointerR = pointerR + 1
      } else {
        removeAddedCharToTargetMap(s(pointerR))
        pointerR = pointerR + 1 //broaden the window on the right
      }
    }

    result
  }

  println(
    minWindow("ADOBECODEBANC", "ABC") //BANC
  )

}
