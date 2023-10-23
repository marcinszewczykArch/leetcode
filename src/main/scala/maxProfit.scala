
object maxProfit extends App {

  def maxProfit(prices: Array[Int]): Int = {
    val map = prices.zipWithIndex
    map.map { case (price, index) =>
        map.splitAt(index)._2
        .map { case (newPrice, _) => (newPrice - price) }.max
    }
      .max
  }

  def maxProfit2(prices: Array[Int]): Int = {
    var min = prices(0)
    var max = prices(0)
    val maxProfit = scala.collection.mutable.ListBuffer[Int](0)

    prices.foreach { i =>
      if (i < min) { maxProfit.addOne(max - min); min = i; max = i }
      if (i > max) { max = i; maxProfit.addOne(max - min) }
    }
    maxProfit.max
  }

  val prices = Array(7,1,5,3,6,4)

  println(
    maxProfit2(prices)
  )




}
