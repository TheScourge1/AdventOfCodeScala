package advent2019

import scala.collection.mutable.ListBuffer

object Day10 extends Day(10){
  override def testSetA = List(TestCase("Day10_testa.txt","(3,4) -> 8"),TestCase("Day10_testa2.txt","(11,13) -> 210"))

  override def testSetB = List()

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
    val rows = map.size
    val cols = map(0).size
    val station = (19,20)

    val boundaryList = ListBuffer[(Int,Int)]()

    for(i <- station._1 until cols) boundaryList+=((i,0))
    for(j <- 0 until rows) boundaryList+=((cols-1,j))
    for(i <- Range(cols-1,0,-1)) boundaryList+=((i,rows-1))
    for(j <- Range(rows-1,0,-1)) boundaryList+=((0,j))
    for(i <- 0 until station._1) boundaryList+=((i,0))

    var laserHits = 0
    val MAXLASERHITS=200
    while(true){
      for(b <- boundaryList){
        val diff = ((b._1-station._1)/ggd(b._1-station._1,b._2-station._2),
                    (b._2-station._2)/ggd(b._1-station._1,b._2-station._2))
        var n=1
        var hit = false
        while(n <= ggd(b._1-station._1,b._2-station._2 )&& !hit) {
          val x = n*(diff._1)+station._1
          val y = n*(diff._2)+station._2
          n+=1
          if(map(x)(y)) {
            map(x)(y) = false
            laserHits +=1
            println("LaserHits: "+laserHits + s" (${x},${y})")
            hit = true
            if(laserHits == MAXLASERHITS) return ((x)*100+y).toString
            if(laserHits == 139) printMatrix(map)
          }
        }
      }
    }
    "ERROR"
  }
}
