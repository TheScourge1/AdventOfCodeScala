package advent2019

import scala.collection.mutable.ListBuffer

object Day10 extends Day(10){
  override def testSetA = List(TestCase("Day10_testa.txt","(3,4) -> 8"),TestCase("Day10_testa2.txt","(11,13) -> 210"))

  override def testSetB = List(TestCase("Day10_testa2.txt","802",List("11,13")))
  override def paramsB = List("19,20")

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

  def printMatrix(map: Array[Array[Boolean]]): Unit ={
    val rows = map.size
    val cols = map(0).size
    for(i<- 0 until rows){println;for(j <- 0 until cols) {if(map(i)(j))print("#")else print(".")}}
  }

  override def solutionB(input: List[String], params: List[String]): String = {
    val map: Array[Array[Boolean]] = input.map(s => s.map(c => if(c.equals('#')) true else false).toArray).toArray
    val station = (params(0).split(",").head.toInt,params(0).split(",").tail.head.toInt)

    var degreeMap = Map[Double,List[(Int,Int)]]()
    for(i <- 0 until map(0).size;j <- 0 until map.size) {
      if(map(i)(j)){
        val degree = calcDegree((i,j),station)
        degreeMap = degreeMap + (degree -> (degreeMap.getOrElse(degree, List[(Int,Int)]()) :+ (i,j)))
      }
    }
/*
    println(s"(5,0),(5,5) -> ${calcDegree((5,0),(5,5))}")
    println(s"(10,0),(5,5) -> ${calcDegree((10,0),(5,5))}")
    println(s"(10,5),(5,5) -> ${calcDegree((10,5),(5,5))}")
    println(s"(10,10),(5,5) -> ${calcDegree((10,10),(5,5))}")
    println(s"(5,10),(5,5) -> ${calcDegree((5,10),(5,5))}")
    println(s"(0,10),(5,5) -> ${calcDegree((0,10),(5,5))}")
    println(s"(0,5),(5,5) -> ${calcDegree((0,5),(5,5))}")
    println(s"(0,0),(5,5) -> ${calcDegree((0,0),(5,5))}")*/

    var laserHits = 0
    for(degree <- degreeMap.keySet.toList.sorted){
      println(s"${degree} -> ${degreeMap.getOrElse(degree,List())}")
      if(degreeMap.get(degree).nonEmpty){
        laserHits +=1
        val currentHit  = degreeMap.getOrElse(degree,List())
                        .sortBy(f => Math.pow(f._1-station._1,2)+Math.pow(f._2-station._2,2)).head
       // println(s"currentHit: ${laserHits} -> ${currentHit}")
        if(laserHits == 200) return (currentHit._2*100+currentHit._1).toString
        degreeMap = degreeMap + (degree -> degreeMap.getOrElse(degree,List())
                                            .sortBy(f => Math.pow(f._1-station._1,2)+Math.pow(f._2-station._2,2)).tail)
      }
    }

    "TODO"
  }

  def calcDegree(x: (Int,Int), y: (Int,Int)): Double = {
    val angle = Math.acos((y._2 - x._2) / Math.sqrt(Math.pow(y._2 - x._2, 2) + Math.pow(y._1 - x._1, 2)))
    if (x._1 >= y._1) angle
    else 2*Math.PI - angle
  }
}
