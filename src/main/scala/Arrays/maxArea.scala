package Arrays

/*
You are given an integer array height of length n. There are n vertical lines drawn such that the two endpoints of the ith line are (i, 0) and (i, height[i]).
Find two lines that together with the x-axis form a container, such that the container contains the most water.
Return the maximum amount of water a container can store.
Notice that you may not slant the container.
*/

object maxArea extends App {

  //bruteforce O(n^2)
  def maxArea(height: Array[Int]): Int = {
    def calculateArea(h1: Int, h2: Int, i1: Int, i2: Int) = math.min(h1, h2) * (i2 - i1)
    val zipped = height.zipWithIndex

    (for {
      (h1, i1) <- zipped
      area = zipped.drop(i1 + 1).foldLeft(0){ case (currA, (h2, i2)) =>
        val nextA = calculateArea(h1, h2, i1, i2)
        if (nextA > currA) nextA else currA
      }
    } yield area).max

  }

  //Optimized Solution O(n) - two pointers pattern
  def maxArea2(height: Array[Int]): Int = {
    def calculateArea(h1: Int, h2: Int, i1: Int, i2: Int) = math.min(h1, h2) * (i2 - i1)

    var i1 = 0
    var i2 = height.length - 1
    var maxArea = 0

    while(i1 != i2) {
      val h1 = height(i1)
      val h2 = height(i2)
      val area = calculateArea(h1, h2, i1, i2)
      println("i1:" + i1 + " h1:" + h1 + " i2: " + i2 + " h2:" + h2 +   " area:" + area)
      if(area > maxArea) maxArea = area

      if(h1 < h2) i1 = i1 + 1 else i2 = i2 - 1
    }



    maxArea
  }


  val height: Array[Int] = Array(1,2,1)
  val height2: Array[Int] = Array(1,8,6,2,5,4,8,3,7)

  println(
    maxArea2(height2)
  )

}
