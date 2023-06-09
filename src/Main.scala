import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.math.{pow, sqrt}
import scala.util.Random
import scala.util.control.Breaks._

object Main {

  /**
   * 导入数据，得到城市坐标信息
   * param dataPath: 数据文件地址 str
   * return: 所有城市的坐标信息 二维 list
   *
   * @return
   */
  private def loadData(dataPath: String): ListBuffer[ArrayBuffer[Int]] = {
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
  private def getTwoCitiesDist(city1: ArrayBuffer[Int], city2: ArrayBuffer[Int]): Double = {
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
  private def getCitiesDistance(cities: ListBuffer[ArrayBuffer[Int]]): Array[Array[Double]] = {
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
  private def print2DArr[T](arr: Array[Array[T]]): Unit = {
    for (row <- arr) {
      for (ele <- row) {
        print(ele + ",")
      }
      println()
    }
  }

  /**
   * 打印一维数组
   * 泛型方法
   */
  private def print1DArr[T](arr: Array[T]): Unit = {
    for (ele <- arr) {
      print(ele + ",")
    }
  }

  /**
   * 自动匹配打印1d/2d数组
   */
  private def printArr[T](arr: Any): Unit = {
    arr match {
      case _: Array[_] =>
        print1DArr(arr.asInstanceOf[Array[T]])
      // 处理一维数组的逻辑
      case _: Array[Array[_]] =>
        // 处理二维数组的逻辑
        print2DArr(arr.asInstanceOf[Array[Array[T]]])
      case _ =>
        println("未知类型")
      // 处理未知类型的逻辑
    }
  }

  /**
   * 利用 Fisher-Yates 算法生成随机不重复的一维数组，表明某个城市到其他城市的路线图
   * Fisher-Yates shuffle - 洗牌算法
   *
   * @param numRoutes 生成的路径数量 numRoutes<=numCities
   * @param numCities 城市数量
   * @return Array 类型
   */
  private def createRandomRoutes(numRoutes: Int, numCities: Int): Array[Int] = {
    val arr = Array.tabulate(numCities)(i => i) // 创建包含[0,numCities-1]的数组
    for (i <- 0 until numRoutes) {
      val j = i + Random.nextInt(numCities - i) // 从剩余元素中随机选择一个索引
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
    }
//    arr.take(numRoutes) // 返回前numRoutes个元素(一维数组)
    arr
  }

  /**
   * 获取城市之间路线图
   *
   * @param numRoutes 路径数量
   * @param numCities 城市数量
   * @return
   */
  private def getRandomRoutes(numRoutes: Int, numCities: Int): ArrayBuffer[Array[Int]] = {
    val routes: ArrayBuffer[Array[Int]] = ArrayBuffer.fill(numRoutes)(Array.ofDim[Int](numCities))
    for (i <- 1 to numRoutes) {
      routes(i - 1) = createRandomRoutes(numRoutes, numCities)
    }
    routes
  }

  /**
   * 获取一条路线的适应度值：使用路线总长度的倒数作为适应度值
   *
   * @param routes     所有路线
   * @param distMatrix 所有两两城市之间距离
   * @return 路线适应度值 Double
   */
  private def getRouteFitnessValue(routes: Array[Int], distMatrix: Array[Array[Double]]): Double = {
    var distSum = 0.0
    val len = routes.length
    for (i <- 0 until len - 1) {
      distSum += distMatrix(routes(i))(routes(i + 1))
    }
    distSum += distMatrix(routes(len - 1))(routes(0))
    1 / distSum
  }

  /**
   * 计算所有路线的适应度
   *
   * @param routes     所有路线 2d_array
   * @param distMatrix 距离矩阵 2d_array
   * @return 所有路线的适应度 1d_array
   */
  private def getAllRoutesFitnessValue(routes: ArrayBuffer[Array[Int]], distMatrix: Array[Array[Double]]) = {
    val len = routes.length
    val fitnessValues: ListBuffer[Double] = ListBuffer.fill(len)(0.0)
    for (i <- 0 until len) {
      fitnessValues(i) = getRouteFitnessValue(routes(i), distMatrix)
    }
    fitnessValues
  }

  /**
   *
   * 首先计算权重数组 probability 的总和 totalWeight。
   * 然后，生成一个随机的双精度浮点数 threshold，其取值范围在 0 到 totalWeight 之间。
   * 接下来，使用循环遍历权重数组，并累加权重值，直到累加值超过或等于 threshold。
   * 循环结束后，返回所选元素的下标 index。
   *
   * //   * @param arr 从给定的下标范围选取生成随机值的样本
   *
   * @param probability 权重一维数组
   * @return 返回下标值
   */
  //  def weightedRandomChoice(arr: Array[Int],probability: Array[Double]): Int = {
  private def weightedRandomChoice(probability: ListBuffer[Double],totalWeight: Double, len: Int): Int = {
    val threshold = Random.nextDouble() * totalWeight
    var cumulativeWeight = 0.0
    var index = 0
    while (index < len - 1 && cumulativeWeight + probability(index) < threshold) {
      cumulativeWeight += probability(index)
      index += 1
    }
    index
  }

  /**
   * 选择操作：
   * 个体路线适应度越高，其被选择的机会就越多。故此处采用与适应度成比例的概率方法进行选择。
   * 具体地说，就是首先计算群体中所有个体适应度的总和，再计算每个个体的适应度所占的比例，
   * 并以此作为相应的选择概率。然后采用轮盘赌方法进行 routes=100 次[带概率的随机选择]。
   *
   * @param routes              所有路线 2d_array
   * @param routesFitnessValues 所有路线适应度 1d_array
   * @return 淘汰掉低适应度后的路线 2d_array
   */
  private def selection(routes: ArrayBuffer[Array[Int]], routesFitnessValues: ListBuffer[Double]): ArrayBuffer[Array[Int]] = {
    // 根据 routes 的长宽大小申请一个同样大小的二维数组
//    val selectedRoutes: ArrayBuffer[Array[Int]] = ArrayBuffer.fill(routes.length)(Array.ofDim[Int](routes(0).length))
      val selectedRoutes: ArrayBuffer[Array[Int]] = ArrayBuffer.fill(routes.length)(Array.fill(routes(0).length)(0))
    val sumFitnessValue = routesFitnessValues.sum
    val probability = routesFitnessValues.map(ele => ele / sumFitnessValue)
    //    printArr(probability.toArray)
    val numRoutes = routes.length
    // numRoutes 次轮盘赌带权重随机选择
    for (i <- 0 until numRoutes) {
      val idx = weightedRandomChoice(probability,probability.sum, probability.length)
      selectedRoutes(i) = routes(idx)
    }
    selectedRoutes
  }

  /**
   *  1. 采用基于部分映射的交配法
   *     2. 产生一个随机数，确定交叉的起始位置，对起始位置及之后的位置进行交换，
   *     并放在首位，然后再从各自的路径中按顺序填充未出现过的数字。
   *     3. 对所有路线两两进行交叉操作。如路线 0 和路线 1，路线 2 和路线 3，以此类推。
   *
   * @param routes    所有路线 2d_array
   * @param numCities 城市数量 int
   * @return 交叉后的所有路线 2d_array
   */
  private def crossover(routes: ArrayBuffer[Array[Int]], numCities: Int): ArrayBuffer[Array[Int]] = {
    val len = routes.length //这里len数值上=numCities
    // 这里要区分len是奇数还是偶数，如果len是奇数则应取n-1，如果len是偶数则取len
    for (i <- 0 until len by 2) {
      // 产生 [0,numCities) 之间的随机整数，作为交叉点的下标
      val segPoint = Random.nextInt(numCities)
      val crossLen = numCities - segPoint
      // 获取需要做交叉的两条路线
      val r1 = routes(i)
      val r2 = routes(i + 1)
      // 获取需要做交叉的片段并做交叉（做两条路线从segPoint往后相同位置的交换）
      val r1Cross = r2.slice(segPoint, r2.length)
      val r2Cross = r1.slice(segPoint, r1.length)
      // 做 r1 与 r1Cross 的交集后,从 r1 中取排除 r1Cross 后的元素，得到r1中未做交叉的元素
      val r1NonCross = r1.diff(r1Cross)
      val r2NonCross = r2.diff(r2Cross)
      // 填充 r1_new 和 r2_new
      routes(i) = routes(i).patch(0, r1Cross, crossLen).patch(crossLen, r1NonCross, r1NonCross.length)
      routes(i + 1) = routes(i+1).patch(0, r2Cross, crossLen).patch(crossLen, r2NonCross, r2NonCross.length)
    }
    routes
  }

  /**
   *
   * 变异操作，变异概率为 0.01
   * 变异操作是按向量维度进行的，即把某一维的内容进行变异。
   * 变异操作同样也是随机进行的。
   * 一般而言，变异概率都取得较小，这里概率取0.01，
   * 采取对于一个路线个体随机选取两个城市进行交换的策略
   * @param routes    所有路线 2d_array
   * @param numCities 城市数量 int
   * @return 变异后的所有路线 2d_array
   */
  private def mutation(routes: ArrayBuffer[Array[Int]], numCities: Int): ArrayBuffer[Array[Int]] = {
    val prob = 0.01
    // 这里 Random.nextDouble() 生成的是[0.0,1.0)伪随机数
    val pRandArr = Array.fill(routes.length)(Random.nextDouble())
    val len = routes.length
    for (i <- 0 until len) {
      if (pRandArr(i) < prob) {
        val mutPosition = createRandomRoutes(2, numCities)
        val (l, r) = (mutPosition(0), mutPosition(1))
        // 使用元祖解构操作
        val temp = (routes(i)(l), routes(i)(r))
        routes(i)(l) = temp._2
        routes(i)(r) = temp._1
      }
    }
    routes
  }

  /**
   * main 方法
   */
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    val numRoutes = 100 // 路线数量
    var numCities = 127 // 城市数量
    val epoch = 100000 // 迭代次数

    // ===========================前期准备===========================
    // 导入数据
    val cities: ListBuffer[ArrayBuffer[Int]] = loadData("./cities.txt")
    numCities = cities.length
    //    numCities.foreach(println)
    // 计算各城市距离矩阵
    val distMatrix: Array[Array[Double]] = getCitiesDistance(cities)
    //    printArr(distArr2D)
    //    获取路线图
    var routes: ArrayBuffer[Array[Int]] = getRandomRoutes(numRoutes, numCities)
    //    printArr(routes.toArray)
    //     获取所有路线的适应度
    var routesFitnessValues = getAllRoutesFitnessValue(routes, distMatrix)
    //    printArr(routesFitnessValues.toArray)
    var (maxValue, bestFitnessIndex) = routesFitnessValues.zipWithIndex.maxBy(_._1)
    var (bestRoute, bestFitness) = (routes(bestFitnessIndex), maxValue)
    //    print(s"最好适应度为：$bestFitness\n最好路线为：${bestRoute.mkString("Array(", ", ", ")")}")

    // ===========================开始迭代===========================
    var notImproveEpoch = 0
    breakable {
      for (i <- 0 until  epoch) {
        routes = selection(routes, routesFitnessValues) // 选择
        routes = crossover(routes, numCities) // 交叉
        routes = mutation(routes, numCities)// 变异
        // 在一轮选择交叉变异后重新计算所有路线适应度以及获取最好适应度下标
        routesFitnessValues = getAllRoutesFitnessValue(routes, distMatrix)
//        val (maxValue, bestFitnessIndex) = routesFitnessValues.zipWithIndex.maxBy(_._1)
        // 从 0 开始往后查找最大索引值
        bestFitnessIndex = routesFitnessValues.indexOf(routesFitnessValues.max, 0)
        if (routesFitnessValues(bestFitnessIndex) > bestFitness) {
          notImproveEpoch = 0
          bestRoute = routes(bestFitnessIndex)
          bestFitness = routesFitnessValues(bestFitnessIndex) // 保存最优路线及其适应度
        } else {
          notImproveEpoch += 1
        }
        // 打印当前最优路线
        if ((i + 1) % 200 == 0) {
          print(s"epoch: ${i+1}\nnotImproveEpoch: ${notImproveEpoch+1}\n最好适应度为：$bestFitness\n当前最优路线为：${bestRoute.mkString("Array(", ", ", ")")}\n当前最优路线距离为：${1/getRouteFitnessValue(bestRoute,distMatrix)}\n\n")
        }
        if (notImproveEpoch >= 2000) {
          print("连续2000次迭代都没有改变最优路线，结束迭代\n")
          break()
        }
      }
    }
    print(s"最优路线为：${bestRoute.mkString("Array(", ", ", ")")}\n总距离为：${1/bestFitness}\n")
    val end = System.currentTimeMillis()
    val msg = f"\n程序运行耗时: ${((end - start) / 1000)+0.0}%.2f s."
    printf(msg)
  }
}
