package String

/*
A phrase is a palindrome if, after converting all uppercase letters into lowercase letters and removing all non-alphanumeric characters, it reads the same forward and backward. Alphanumeric characters include letters and numbers.
Given a string s, return true if it is a palindrome, or false otherwise.

Example 1:
Input: s = "A man, a plan, a canal: Panama"
Output: true
Explanation: "amanaplanacanalpanama" is a palindrome.

Example 2:
Input: s = "race a car"
Output: false
Explanation: "raceacar" is not a palindrome.

Example 3:
Input: s = " "
Output: true
Explanation: s is an empty string "" after removing non-alphanumeric characters.
Since an empty string reads the same forward and backward, it is a palindrome.

*/

object isPalindrome extends App {

  def isPalindrome(s: String): Boolean = {
    val array = s.toUpperCase.toCharArray.filter(c => c.isLetter || c.isDigit)
    var i1 = 0
    var i2 = array.length - 1

    while (i1 <= i2) {
      if(array(i1) != array(i2))
        return false
      i1 = i1 + 1
      i2 = i2 - 1
    }
      true
  }

  println(
    isPalindrome("A man, a plan, a canal: Panama")
  )

}
