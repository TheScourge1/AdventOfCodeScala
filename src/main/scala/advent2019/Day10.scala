package advent2019

import scala.collection.mutable.ListBuffer

object Day10 extends Day(10){
  override def testSetA = List(TestCase("Day10_testa.txt","(3,4) -> 8"),TestCase("Day10_testa2.txt","(11,13) -> 210"))

  override def testSetB = List(TestCase("Day10_testa2.txt","802",List("11,13")))
  override def paramsB = List("20,19")

  override def solutionA(input: List[String], params: List[String]) = {
    val map: Array[Array[Boolean]] = input.map(s => s.map(c => if(c.equals('#')) true else false).toArray).toArray
    val cols = map(0).size
    val rows = map.size

    val resultArr = Array.ofDim[Int](rows,cols)
    var maxRes = 0
    var result = (0,0)
    for(i <- 0 until rows;j <- 0 until cols) {
      if(map(i)(j)) resultArr(i)(j) = countAstroids(i, j, map)
      if(resultArr(i)(j) > maxRes) {maxRes = resultArr(i)(j); result = (j,i)}
    }

    result.toString + " -> " + (maxRes)
  }

  override def solutionB(input: List[String], params: List[String]): String = {
    val map: Array[Array[Boolean]] = input.map(s => s.map(c => if(c.equals('#')) true else false).toArray).toArray
    val station = (params(0).split(",").head.toInt,params(0).split(",").tail.head.toInt)

    var degreeMap = Map[Double,List[(Int,Int)]]()
    for(i <- 0 until map(0).size;j <- 0 until map.size) {
      if(map(j)(i)){
        val degree = calcDegree((i,j),station)
        degreeMap = degreeMap + (degree -> (degreeMap.getOrElse(degree, List[(Int,Int)]()) :+ (i,j)))
      }
    }

    var laserHits = 0
    for(degree <- degreeMap.keySet.toList.sorted){ // only simulation one laser run
      if(degreeMap.get(degree).nonEmpty){
        laserHits +=1
        val currentHit  = degreeMap.getOrElse(degree,List())
                        .sortBy(f => Math.pow(f._1-station._1,2)+Math.pow(f._2-station._2,2)).head
        if(laserHits == 200) return (currentHit._1*100+currentHit._2).toString
        degreeMap = degreeMap + (degree -> degreeMap.getOrElse(degree,List())
                                            .sortBy(f => Math.pow(f._1-station._1,2)+Math.pow(f._2-station._2,2)).tail)
      }
    }

    "ERROR"
  }

  def countAstroids(row: Int,col:Int,map:  Array[Array[Boolean]]): Int ={
    var count = 0
    for(i <- 0 until map(0).size;j<- 0 until map.size){
      if((i != row || j != col) && map(i)(j)) {
        val diff = ((row-i)/ggd(row-i,col-j),(col-j)/ggd(row-i,col-j))
        var n=1
        var blocked = false
        while(n < ggd(row-i,col-j)) {
          if(map(n*(diff._1)+i)(n*(diff._2)+j) ) blocked = true
          n+=1
        }
        if(!blocked) count+=1
      }
    }

    count
  }

  def ggd(a:Int, b:Int):Int = {
    if(a < 0) ggd(-a,b)
    else if(b < 0) ggd(a,-b)
    else if(a == 0 || b == 0) a+b
    else if(a==b) a
    else{
      var res = 1
      for(n <- 2 to Math.min(a,b))
        if(a%n == 0 && b%n == 0) res = n
      res
    }

  }

  def calcDegree(x: (Int,Int), y: (Int,Int)): Double = {
    val angle = Math.acos((y._2 - x._2) / Math.sqrt(Math.pow(y._2 - x._2, 2) + Math.pow(y._1 - x._1, 2)))
    if (x._1 >= y._1) (angle*100000).round/100000.toDouble
    else ((2*Math.PI - angle)*100000).round/100000.toDouble
  }
}
