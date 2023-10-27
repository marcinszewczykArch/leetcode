package String

/*
Given two strings s and t, return true if t is an anagram of s, and false otherwise.
An Anagram is a word or phrase formed by rearranging the letters of a different word or phrase, typically using all the original letters exactly once.

Example 1:
Input: s = "anagram", t = "nagaram"
Output: true

Example 2:
Input: s = "rat", t = "car"
Output: false
*/

object isAnagram extends App {

  def isAnagram(s: String, t: String): Boolean = s.toCharArray.sorted sameElements t.toCharArray.sorted


  println(
    isAnagram("anagram", "nagaram")
  )

}
