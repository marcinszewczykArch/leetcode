
object threeSum extends App {

  val nums: Array[Int] =
    Array(-1,0,1,2,-1,-4)

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val zipped = nums.zipWithIndex
    val result =  scala.collection.mutable.ListBuffer.empty[List[Int]]
    for {
      (num1, i1) <- zipped
      (num2, i2) <- zipped.filter { case (_, i2) => i1 != i2 }
      _          <- zipped.map { case (num3, i3) =>
          if(0 == num1 + num2 + num3 && i1 != i3 && i2 != i3) result.addOne(List(num1, num2, num3)) }
    } yield ()

    result.toList.foldLeft(List.empty[List[Int]]) { case (acc, list) =>
      if (!acc.map(l => l.sorted).contains(list.sorted)) acc :+ list else acc
    }
  }

  def threeSum2(nums: Array[Int]): List[List[Int]] = {
    val map = nums.zipWithIndex.toMap
    (
      for {
        i1 <- LazyList.range(0, nums.length-2)
        i2 <- LazyList.range(i1 + 1, nums.length-1)
        num1 = nums(i1)
        num2 = nums(i2)
        num3 = -(num1 + num2)
        i3 = map.get(num3)
        if i3.isDefined && i3.get != i1 && i3.get != i2
        result = List(num1, num2, num3).sorted
    } yield result
      ).distinct.toList
  }

  println(
    threeSum2(nums).mkString("[",",","]")
  )

}
