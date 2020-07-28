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

    val startOptions = distanceGrid.getNeighbours(Key('@'))
      .filter(k => distanceGrid.getVisitConditions(Key('@'),k).isEmpty)
      .toList
    val path = findShortestPath(startOptions.toSet,List(Key('@')),distanceGrid.getNeighbours(Key('@')) -- startOptions,0,distanceGrid)

    println("Result: "+path._2.toString)
    path._2.toString
  }

  def findShortestPath(toVisit: Set[Key],visited: List[Key],cannotVisitYet: Set[Key],currentCost: Int,distanceGrid:Grid): (List[Key],Int) ={
    if(cannotVisitYet.size == 0 && toVisit.size == 0) return (visited,currentCost)
    else if(toVisit.size == 0) return (List(),-1)

    val currentKey = visited.last
    var bestPath = List[Key]()
    var optimalCost = -1
    for(nextKey <- toVisit){
      val newKeyOptions = distanceGrid.getNewReachOptions(visited,nextKey)
      val newPathOption = findShortestPath((toVisit - nextKey)++ newKeyOptions,visited :+ nextKey,cannotVisitYet -- newKeyOptions,
        currentCost+distanceGrid.getVisitCost(currentKey,nextKey),distanceGrid)

      if(newPathOption._2 != -1 && (optimalCost == -1 || optimalCost > newPathOption._2)){
        bestPath = newPathOption._1
        optimalCost = newPathOption._2
      }
    }

    println(bestPath.toString+": "+optimalCost)
    (bestPath,optimalCost)
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

  def findClosedKeys(path: List[(Int,Int)],doors: List[Door],matrix: Array[Array[Char]], grid: Grid): Grid = {
    val startKey = Key(matrix(path.head._1)(path.head._2))
    val currentLoc = path.last
    var newGrid = grid

    def isValidMove(location: (Int, Int), matrix: Array[Array[Char]]) =
      location._1 > 0 && location._1 < matrix.size &&
        location._2 > 0 && location._2 < matrix(0).size &&
        matrix(location._1)(location._2) != '#'

    var newLocations = List[(Int, Int)]()
    for (x <- -1 to 1; y <- -1 to 1)
      if(Math.abs(x) + Math.abs(y) == 1){
        val newLoc = (x + currentLoc._1, y + currentLoc._2)
        if(isValidMove(newLoc, matrix) && !path.contains(newLoc)) newLocations = newLocations :+ newLoc
      }

    for (loc <- newLocations) {
      val newVal = matrix(loc._1)(loc._2)
      if (isKey(newVal)) newGrid =
        findClosedKeys(path :+ loc,doors,matrix,newGrid.addMove(startKey, Key(newVal), path.size, doors))
      else if (isDoor(newVal)) newGrid = findClosedKeys(path :+ loc, doors :+ Door(newVal), matrix, newGrid)
      else newGrid = findClosedKeys(path :+ loc, doors, matrix, newGrid)
    }
    newGrid
  }

  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }

  def isKey(c:Char) =  c >= 'a' &&  c <= 'z'
  def isDoor(c:Char) =  c >= 'A' &&  c <= 'Z'
  def isDoorKey(k: Key, d: Door) = k.c - 'a' + 'A' == d.c
  def toDoorKey(d: Door) = Key((d.c - 'A' + 'a').toChar)

  def printMatrix(distArr: Array[Array[Int]]) = {
    for(i<- distArr.indices) {
      for(j<- distArr.indices) print(distArr(i)(j)+"\t")
      println
    }
  }

  case class Key(c: Char){ override def toString() = c.toString}
  case class Door(c: Char){ override def toString() = c.toString}

  class Grid(grid: Map[Key,Map[Key,(Int,List[Door])]] = Map()) {

    private def getFromKeyOptions(fromKey: Key) = grid.getOrElse(fromKey, Map[Key, (Int, List[Door])]())
    private def getVisitCostConditions(fromKey: Key, toKey: Key) = getFromKeyOptions(fromKey).getOrElse(toKey, (-1, List()))

    def getNeighbours(key: Key): Set[Key] = grid.getOrElse(key, Map[Key, (Int, List[Door])]()).keySet
    def getVisitCost(fromKey: Key, toKey: Key): Int = getVisitCostConditions(fromKey,toKey)._1
    def getVisitConditions(fromKey: Key, toKey: Key): List[Door] = getVisitCostConditions(fromKey,toKey)._2

    def getNewReachOptions(visited: List[Key],newKey: Key): List[Key] = {
      val fromStartConditions = getFromKeyOptions(Key('@'))
      var result = List[Key]()
      for(key <- fromStartConditions.keySet) {
        val keysNeeded = fromStartConditions.get(key).get._2.map(d => toDoorKey(d))
        if(keysNeeded.contains(newKey) && (keysNeeded diff visited).size == 1) result = result :+ key
      }
      result
    }

    def addMove(fromKey: Key, toKey: Key, cost: Int, conditions: List[Door]): Grid =
      new Grid(grid + (fromKey -> getFromKeyOptions(fromKey).+(toKey -> (cost, conditions))))

    def removeKey(key: Key) = new Grid(grid - key)
    def removeKeys(keys: Set[Key]) = new Grid(grid -- keys)
    def getKeys(): Set[Key] = grid.keySet

    override def toString(): String = {
      def printKey(key: Key): String = s"${key}: "
      def printPathCost(fromKey: Key, toKey: Key) = s"${toKey}(${grid.get(fromKey).get(toKey)._1}) "
      def printPathConditions(fromKey: Key, toKey: Key) = s"|(${grid.get(fromKey).get(toKey)._2.foldRight("")(_ + "," + _)}); "
      val b = new StringBuilder()
      for (fromKey <- grid.keySet) {
        b ++= printKey(fromKey)
        grid.get(fromKey).get.keySet.foreach(toKey => b++=printPathCost(fromKey, toKey) + printPathConditions(fromKey, toKey))
        b++= "\n"
      }
      b.toString()
    }
  }
}
