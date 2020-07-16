package advent2019

import scala.collection.mutable.ListBuffer

object Day18 extends Day(18){
  override def testSetA = List(TestCase("Day18_testa.txt","86"),TestCase("Day18_testa2.txt","132"),TestCase("Day18_testa3.txt","136"))

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val matrix = input.map(s => s.toArray).toArray
    val keySet = getAllKeys(matrix).toSet + Key('@')

    var distanceGrid = new Grid()
    for(key <- keySet ) distanceGrid = findClosedKeys(List(findLocation(key,matrix)),List[Door](),matrix,distanceGrid)
    println(distanceGrid.toString())
    val res = calcGridMatrices(distanceGrid)
    val keyRes = res._1
    val costResArr = res._2
    val condResArr = Array.ofDim[List[Int]](keyRes.size,keyRes.size)
    for(i<-0 until keyRes.size;j <- 0 until keyRes.size) condResArr(i)(j) = res._3(i)(j).map(k => keyRes.indexOf(k))
    println(res._1)
    printMatrix(res._2)

    val resList = findShortestPath(List(res._1.indexOf(Key('@'))), costResArr,condResArr,0,0)
    val pathCosts = (for(j <- 0 until resList.size-1) yield res._2(resList(j))(resList(j+1)))
    println("Path:"+resList.toString)
    println("Costs"+pathCosts.toString)
    pathCosts.sum.toString
  }

  def findStartLocation(matrix: Array[Array[Char]]) = findLocation(Key('@'),matrix)

  def findLocation(key: Key,matrix: Array[Array[Char]]): (Int,Int) = {
    for(i<- 0 until matrix.size; j<- 0 until matrix(0).size)
      if(matrix(i)(j) == key.c) return (i,j)

    (-1,-1)
  }

  def getAllKeys(matrix: Array[Array[Char]]): List[Key] = {
    var result = ListBuffer[Key]()
    for(i<- 0 until  matrix.size; j<- 0 until matrix(0).size)
      if(isKey(matrix(i)(j))) result += Key(matrix(i)(j))
    result.toList
  }

  def getAllDoors(matrix: Array[Array[Char]]): List[Door] = {
    var result = ListBuffer[Door]()
    for(i<- 0 until  matrix.size; j<- 0 until matrix(0).size)
      if(isDoor(matrix(i)(j))) result += Door(matrix(i)(j))
    result.toList
  }

  def findClosedKeys(steps: List[(Int,Int)],doors: List[Door],matrix: Array[Array[Char]], grid: Grid): Grid = {
      val startKey = Key(matrix(steps.head._1)(steps.head._2))
      val currentLoc = steps.last
      var newGrid = grid

      def isValidMove(location: (Int,Int),matrix: Array[Array[Char]])=
        location._1 > 0 && location._1 < matrix.size &&
        location._2 > 0 &&  location._2 < matrix(0).size &&
        matrix(location._1)(location._2) != '#'

      var newLocations = List[(Int,Int)]()
      for(i <- -1 to 1; j <- -1 to 1) {
        val newLoc = (i + currentLoc._1, j + currentLoc._2)
        if (Math.abs(i) + Math.abs(j) == 1 && isValidMove(newLoc, matrix) && !steps.contains(newLoc))
          newLocations = newLocations :+ newLoc
      }

      for(loc <- newLocations) {
        val newVal = matrix(loc._1)(loc._2)
        if(isKey(newVal)) newGrid = newGrid.addMove(startKey,Key(newVal),steps.size,doors,false)
        else if(isDoor(newVal)) newGrid = findClosedKeys(steps:+loc,doors :+ Door(newVal),matrix,newGrid)
        else  newGrid = findClosedKeys(steps:+loc,doors,matrix,newGrid)
        }
    newGrid
  }

  def calcGridMatrices(grid: Grid): (List[Key],Array[Array[Int]],Array[Array[List[Key]]]) = {
    val keyList = grid.getKeys().toList
    val pathCostArr = Array.ofDim[Int](keyList.size,keyList.size)
    val conditionsArr = Array.tabulate(keyList.size,keyList.size)((i,j) => List[Key]())

    def doStep(path: List[Key],visitCost: Int, doors: List[Door], grid: Grid): Map[Key,(Int,List[Door])] = {
      val newOptions = grid.getNeighbours(path.last)
      var result = Map[Key,(Int,List[Door])]()
      for(key <- newOptions -- path){
            val stepCost = grid.getVisitCost(path.last,key)
            result = result + (key -> (visitCost + stepCost._1,doors ++ stepCost._2))
            result = result ++ doStep(path :+ key,result.get(key).get._1,result.get(key).get._2,grid)
        }
      result
      }

    for(i <- 0 until keyList.size){
      val visitCosts = doStep(List(keyList(i)),0,List(),grid)
      for(j <- 0 until keyList.size){
        if(i != j && keyList(j) != Key('@')) {
          pathCostArr(i)(j) = visitCosts.get(keyList(j)).get._1
          conditionsArr(i)(j) = visitCosts.get(keyList(j)).get._2.map(d => toDoorKey(d))
        }
      }
    }
    (keyList,pathCostArr,conditionsArr)
  }

  def findShortestPath(path: List[Int],distArr: Array[Array[Int]], condArr: Array[Array[List[Int]]]
                       ,minPathSize:Int,currPathSize: Int): List[Int] =  {
    if(path.size == distArr.size) return path

    val currLoc = path.last
    var newPath = List[Int]()
    var newSize = 0
    var newMinSize = minPathSize
    for(i <- 0 until distArr.size){
      if(!path.contains(i) && (minPathSize == 0 || minPathSize > currPathSize + distArr(currLoc)(i)) && (condArr(currLoc)(i).diff(path).size == 0)){
        //println("Path: "+path + " - conditions: "+condArr(currLoc)(i) + "-> "+condArr(currLoc)(i).diff(path))

        val tmpPath = findShortestPath(path :+ i,distArr,condArr,newMinSize,currPathSize + distArr(currLoc)(i))
        val tmpSize = (for(j <- 0 until tmpPath.size-1) yield distArr(tmpPath(j))(tmpPath(j+1))).sum
        if(newSize == 0 || tmpSize < newSize){
          newPath = tmpPath
          newSize = tmpSize
          if(newSize < newMinSize) newMinSize = newSize
        }
      }
    }

    val costs = for(j <- 0 until newPath.size-1) yield distArr(newPath(j))(newPath(j+1))
    println("Path: "+newPath + " - "+costs +" cost: "+newSize)
    newPath
  }

/*
  def findShortestPath(path: List[(Key,Int)],distanceGrid: Grid,minCost: Int): List[(Key,Int)] = {
    if(path.size == distanceGrid.getKeys().size) return path
    val location = path.last._1
    val pathOptions = distanceGrid.getNeighbours(location).union(distanceGrid.getKeys())
    val pathCost = path.map(_._2).sum

    var result = List[(Key,Int)]()
    var optimumCost = minCost

    for(p <- pathOptions){
      val visitConditions = distanceGrid.getVisitCost(location,p)
      if(!path.map(v => v._1).contains(p) &&
        (optimumCost == -1 || pathCost + visitConditions._1 < optimumCost) &&
        hasKeys(path.map(l => l._1),visitConditions._2)) {
        val newPath = findShortestPath(path :+ (p ,visitConditions._1),distanceGrid,optimumCost)
        if(newPath.size > 0 && (result.size == 0 || result.map(l => l._2).sum > newPath.map(l => l._2).sum)) {
          result = newPath
          optimumCost = newPath.map(l => l._2).sum
        }
      }
    }

    if(result.size > 0 ) println(result.map(_._1.c)+ ": "+result.map(_._2).sum)
    else (println("Stopped at: "+path.map(_._1.c)))
    result
  }
*/
  def hasKeys(keys: Iterable[Key],doors: Iterable[Door]): Boolean = {
    val keyChars = keys.map(k => k.c).toList
    val doorChars = doors.map(k => k.c).toList

    for(door <- doorChars)
      if(!keyChars.contains('a' + door - 'A')) return false
    true
  }


  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }

  def isKey(c:Char) =  c >= 'a' &&  c <= 'z'
  def isDoor(c:Char) =  c >= 'A' &&  c <= 'Z'
  def isDoorKey(k: Key, d: Door) = k.c - 'a' + 'A' == d.c
  def toDoorKey(d: Door) = new Key((d.c - 'A' + 'a').toChar)

  def sortedKeySet(l: List[Key]):String = l.map(_.c.toString).sorted.fold("")(_+_)

  def printMatrix(distArr: Array[Array[Int]]) = {
    for(i<- 0 until distArr.size) {
      for(j<-0 until distArr.size) print(distArr(i)(j)+"\t")
      println
    }
  }

  case class Key(c: Char){ override def toString() = c.toString}
  case class Door(c: Char){ override def toString() = c.toString}

  class Grid(grid: Map[Key,Map[Key,(Int,List[Door],Boolean)]] = Map()) {

    def getNeighbours(key: Key):Set[Key] = grid.getOrElse(key,Map[Key,(Int,List[Door],Boolean)]()).keySet
    def getPreferedNeighbours(key: Key):Set[Key] =
      grid.getOrElse(key,Map[Key,(Int,List[Door],Boolean)]()).filter(f => f._2._3).keySet
    def getVisitCost(fromKey:Key,toKey:Key): (Int,List[Door],Boolean)
      = grid.getOrElse(fromKey,Map[Key,(Int,List[Door],Boolean)]()).getOrElse(toKey,(-1,List(),false))

    def addMove(fromKey:Key, toKey: Key,cost:Int, conditions: List[Door],prefered: Boolean): Grid = {
      new Grid(grid + (fromKey -> grid.getOrElse(fromKey,Map[Key,(Int,List[Door],Boolean)]()). + (toKey -> (cost,conditions,prefered))))
    }

    def removeKey(key: Key) = new Grid(grid - key)
    def removeKeys(keys: Set[Key]) = new Grid(grid -- keys)

    def getKeys(): Set[Key] = grid.keySet

     override def toString():String = {
        val b = new StringBuilder()
        for(key <- grid.keySet) {
          b.append(s"${key}: ")
          for(targetKey <- grid.get(key).get.keySet) b.append(s"${targetKey}(${grid.get(key).get(targetKey)._1})|"
            + s"(${grid.get(key).get(targetKey)._2.foldRight("")(_+","+_)}) ")
          b.append("\n")
        }
        b.toString()
    }
  }
}
