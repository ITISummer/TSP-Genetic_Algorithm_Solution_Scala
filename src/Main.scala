import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.math.{pow, round, sqrt}
import scala.util.Random

object Main {

  /**
   * 导入数据，得到城市坐标信息
   * param dataPath: 数据文件地址 str
   * return: 所有城市的坐标信息 二维 list
   *
   * @return
   */
  def loadData(dataPath: String): ListBuffer[ArrayBuffer[Int]] = {
    val res: ListBuffer[ArrayBuffer[Int]] = ListBuffer.empty[ArrayBuffer[Int]]

    Source.fromFile(dataPath).getLines().foreach(line => {
      val splits = line.trim().split("\\s+")
      res += ArrayBuffer(splits(1).toInt, splits(2).toInt)
    })
    res
  }

  /**
   * 具体计算两点（城市）间距离的函数，
   * 距离定义比较简单，两个城市m和n之间的距离定义为二维空间欧氏距离
   */
  def getTwoCitiesDist(city1: ArrayBuffer[Int], city2: ArrayBuffer[Int]): Double = {
    val dx = city1(0) - city2(0)
    val dy = city1(1) - city2(1)
    sqrt(pow(dx, 2) + pow(dy, 2))
  }

  /**
   * 计算所有两两城市的距离
   *
   * @param cities 各城市坐标（二维 list）
   * @return
   */
  def getCitiesDistance(cities: ListBuffer[ArrayBuffer[Int]]): Array[Array[Double]] = {
    val distMatrix = ListBuffer.fill(cities.length)(ArrayBuffer.fill(cities.length)(0.0))
    //将 ListBuffer[ArrayBuffer[]] 转换为二维数组，这里其实可以不替换，但是为了用得更轻松点（不用写很长的变量名），所以就替换下
    val array2D: Array[Array[Double]] = distMatrix.map(_.toArray).toArray
    // 使用双重 for 循环遍历，并带有下标
    for (i <- array2D.indices; j <- i + 1 until cities.length) {
      val dist: Double = getTwoCitiesDist(cities(i), cities(j))
      array2D(i)(j) = dist
      array2D(j)(i) = dist
    }
    array2D
  }

  /**
   * 打印二维数组
   * 泛型方法
   */
  def printArr[T](arr: Array[Array[T]]): Unit = {
    for (row <- arr) {
      for (ele <- row) {
        print(ele+",")
      }
      println()
    }
  }

  /**
   *
    随机初始化路线
      这里[初始种群]为随机生成的 100 条路线。
      使用 np.random.choice 生成 0 至 n-1 的不重复的长度为 n 的 numpy 数组。
    :param n_route: 初始化的路线数量 int
    :param n_cities: 城市的数量 int
    :return: 路线矩阵 二维 nd_array
   * @param numRoute 初始化的路线数量
   * @param numCities 城市的数量
   * @return 路线二维矩阵
   */
  def initRoute(numRoute:Int, numCities: Int): Array[Array[Int]] = {
    val routes = ListBuffer.fill(numRoute)(ArrayBuffer.fill(numCities)(0))
    for(i <- 0 until numRoute) {
//      val random = new Random()
      // 生成一个范围为 [0, numCities-1] 的随机不重复数字集合-洗牌算法
//val randomNumbers = Random.shuffle(0 until numCities).take(numCities).toSet
      // 打印选出的随机数字
//randomNumbers.foreach(println)
    }
    null
  }

  /**
   * 利用 Fisher-Yates 算法生成随机不重复的一维数组，表明某个城市到其他城市的路线图
   * @param numRoutes 生成的路径数量 numRoutes<=numCities
   * @param numCities 城市数量
   * @return Array 类型
   */
  def createRandomRoutes(numRoutes: Int, numCities: Int): Array[Int] = {
    val arr = Array.tabulate(numCities)(i => i) // 创建包含[0,numCities-1]的数组
    val random = new Random()

    for (i <- 0 until numRoutes) {
      val j = i + random.nextInt(numCities - i) // 从剩余元素中随机选择一个索引
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }
//    arr.take(numRoutes) // 返回前numRoutes个元素(一维数组)
    arr
  }

  /**
   * 获取城市之间路线图
   * @param numRoutes 路径数量
   * @param numCities 城市数量
   * @return
   */
  def getRandomRoutes(numRoutes: Int, numCities: Int): ArrayBuffer[Array[Int]] = {
    val routes: ArrayBuffer[Array[Int]] = ArrayBuffer.fill(numRoutes)(Array.ofDim[Int](numCities))
    for (i <- 1 to numRoutes) {
      routes(i-1) = createRandomRoutes(numRoutes, numCities)
    }
    routes
  }
  /**
    计算所有路线的适应度
   * @param routes 所有路线 2d_array
   * @param dist_matrix 距离矩阵 2d_array
   * @return 所有路线的适应度 2d_array
   */
  def getAllRoutesFitnessValue(routes: ArrayBuffer[Array[Int]], distMatrix: ListBuffer[ArrayBuffer[Double]]) = {

    //    fitness_values = np.zeros(len(routes))
    //    for i in range(len(routes)):
    //      f_value = get_route_fitness_value(routes[i], dist_matrix)
    //    fitness_values[i] = f_value
    //    return fitness_values

  }

  /**
   * main 方法
   */
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    val numRoutes = 100 // 路线数量
    val numCities = 127 // 城市数量
    val epoch = 100000 // 迭代次数

    // 导入数据
    val cities: ListBuffer[ArrayBuffer[Int]] = loadData("./cities.txt")
    //    numCities.foreach(println)
    // 计算各城市距离矩阵
    val distArr2D: Array[Array[Double]] = getCitiesDistance(cities)
//    printArr(distArr2D)
//    获取路线图
    val routes: ArrayBuffer[Array[Int]] = getRandomRoutes(numRoutes, numCities)
//    printArr(routes.toArray)
//     获取所有路线的适应度

    val end = System.currentTimeMillis()
    //    [scala string format用法](https://juejin.cn/s/scala%20string%20format%E7%94%A8%E6%B3%95)
    val resSec = (end - start) / 1000
    val msg = f"耗时: $resSec%.2f s."
    printf(msg)
  }
}
