import scala.util.Random

/**
 * [scala随机数生成，生成一组不重复随机数算法](https://blog.csdn.net/springlustre/article/details/48828507)
 * [生成不超过某个数的,不重复的10个随机数](https://segmentfault.com/q/1010000002970273)
 */
object FisherYates {
  def main(args: Array[String]): Unit = {
    // n >= m
    val n = 10 // 总数
    val m = 10 // 选取的个数

    for (i <- 1 to n) {
      val randomNumbers = selectRandomNumbers(n, m)
      randomNumbers.mkString(",").foreach(print)
      println()
    }
  }

  def selectRandomNumbers(n: Int, m: Int): Array[Int] = {
    val arr = Array.tabulate(n)(i => i) // 创建包含0到n-1的数组
    val random = new Random()

    for (i <- 0 until m) {
      val j = i + random.nextInt(n - i) // 从剩余元素中随机选择一个索引
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }

    arr.take(m) // 返回前m个元素
  }


}
