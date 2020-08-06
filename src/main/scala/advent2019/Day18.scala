package advent2019

import scala.collection.mutable.ListBuffer

object Day18 extends Day(18){
  override def testSetA = List(TestCase("Day18_testa.txt","86"),TestCase("Day18_testa2.txt","132"),TestCase("Day18_testa3.txt","136"),TestCase("Day18_testa4.txt","81"))

  override def testSetB = List(TestCase("Day18_testb.txt","8"),TestCase("Day18_testb1.txt","24"),TestCase("Day18_testb2.txt","32"),TestCase("Day18_testb3.txt","72"))

  override def solutionA(input: List[String], params: List[String]) = {
    val matrix = input.map(s => s.toArray).toArray
    val keySet = getAllKeys(matrix).toSet + Key('@',true)

    var distanceGrid = new Grid()
    for(key <- keySet ) distanceGrid = findClosedKeys(List(findLocation(key,matrix)),List[Door](),matrix,distanceGrid)
    println(distanceGrid.toString())

    val startOptions = distanceGrid.getNeighbours(Key('@',true))
      .filter(k => distanceGrid.getVisitConditions(Key('@',true),k).isEmpty)
      .toList
    val path = findShortestPathA(startOptions.toSet,List(Key('@',true)),
      distanceGrid.getNeighbours(Key('@',true)) -- startOptions,distanceGrid,new Buffer())

    println("Result: "+path._1.reverse+" - "+path._2.toString)
    path._2.toString
  }

  def findShortestPathA(toVisit: Set[Key],visited: List[Key],cannotVisitYet: Set[Key],
                       distanceGrid:Grid,buffer: Buffer): (List[Key],Int) = {
    if(cannotVisitYet.size == 0 && toVisit.size == 0) return (List(),0)
    else if(toVisit.size == 0) return (List(),-1)

    val currentKey = visited.last
    if(buffer.getFromCache(currentKey,toVisit++cannotVisitYet).isDefined)
      return buffer.getFromCache(currentKey,toVisit++cannotVisitYet).get

    var bestPath = List[Key]()
    var optimalCost = -1
    for(nextKey <- toVisit){
      val newKeyOptions = distanceGrid.getNewReachOptions(visited,nextKey)
      val newPathOption = findShortestPathA((toVisit - nextKey)++ newKeyOptions,
        visited :+ nextKey,cannotVisitYet -- newKeyOptions,distanceGrid,buffer)

      if(newPathOption._2 != -1 && (optimalCost == -1 || optimalCost > newPathOption._2+distanceGrid.getVisitCost(currentKey,nextKey))){
        bestPath = newPathOption._1 :+ nextKey
        optimalCost = newPathOption._2 + distanceGrid.getVisitCost(currentKey,nextKey)
      }
    }

    buffer.addToCache(currentKey,toVisit-currentKey++cannotVisitYet,bestPath,optimalCost)
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

  def getAllStartLocations(matrix: Array[Array[Char]]) : List[Key] = {
    var result = ListBuffer[Key]()
    for(i<- 0 until  matrix.size; j<- 0 until matrix(0).size)
      if(isStart(matrix(i)(j))) result += Key(matrix(i)(j),true)
    result.toList
  }

  def findClosedKeys(path: List[(Int,Int)],doors: List[Door],matrix: Array[Array[Char]], grid: Grid): Grid = {
    val startKey = Key(matrix(path.head._1)(path.head._2),isStart(matrix(path.head._1)(path.head._2)))
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
    if(!newGrid.getKeys().contains(startKey)) newGrid = newGrid.addKey(startKey)
    newGrid
  }

  override def solutionB(input: List[String], params: List[String]) = {
    var matrix = input.map(s => s.toArray).toArray
    matrix = correctCenter(matrix)

    val distanceGrid = calcDistanceGrid(matrix)
    val startKeys = distanceGrid.getKeys().filter(k => k.isStartLocation)
    var reachableFromStart = Set[Key]()
    for(startKey <- startKeys)
      reachableFromStart = reachableFromStart ++
        distanceGrid.getNeighbours(startKey)
          .filter(k => distanceGrid.getVisitConditions(startKey,k).isEmpty).toList

    val path = findShortestPathB(startKeys,reachableFromStart,
      startKeys.toList,distanceGrid.getKeys() -- startKeys.union(reachableFromStart),distanceGrid,new Buffer())

   // println("Result: "+path._1.reverse+" - "+path._2.toString)
    path._2.toString
  }

  def findShortestPathB(currentLocations: Set[Key],toVisit: Set[Key],visited: List[Key],cannotVisitYet: Set[Key],
                        distanceGrid:Grid,buffer: Buffer): (List[Key],Int) = {

    if(cannotVisitYet.size == 0 && toVisit.size == 0) return (List(),0)
    else if(toVisit.size == 0)
      return (List(),-1)

    if(buffer.getFromCache(currentLocations,toVisit++cannotVisitYet).isDefined)
        return buffer.getFromCache(currentLocations,toVisit++cannotVisitYet).get

    var bestPath = List[Key]()
    var optimalCost = -1
    for(nextKey <- toVisit){
      val currentKey = currentLocations.filter(c => distanceGrid.getNeighbours(c).contains(nextKey)).head
      val newKeyOptions = distanceGrid.getNewReachOptions(visited,nextKey)
      val newPathOption = findShortestPathB((currentLocations+nextKey)-currentKey,(toVisit - nextKey)++ newKeyOptions,
        visited :+ nextKey,cannotVisitYet -- newKeyOptions,distanceGrid,buffer)

      if(newPathOption._2 != -1 && (optimalCost == -1 || optimalCost > newPathOption._2+distanceGrid.getVisitCost(currentKey,nextKey))){
        bestPath = newPathOption._1 :+ nextKey
        optimalCost = newPathOption._2 + distanceGrid.getVisitCost(currentKey,nextKey)
      }
    }

    buffer.addToCache(currentLocations,toVisit--currentLocations++cannotVisitYet,bestPath,optimalCost)
    (bestPath,optimalCost)
  }


  def correctCenter(arr: Array[Array[Char]]): Array[Array[Char]] = {
    val corr = Array(Array('1','#','2'),Array('#','#','#'),Array('3','#','4'))
    val rWith = arr.size/2
    val cWith = arr(0).size/2
    for(i <- -1 to 1; j <- -1 to 1) arr(rWith+i)(cWith+j) = corr(i+1)(j+1)
    arr
  }

  def calcDistanceGrid(arr: Array[Array[Char]]): Grid = {
    val keySet = getAllKeys(arr).toSet ++ getAllStartLocations(arr).toSet
    var distanceGrid = new Grid()
    for(key <- keySet ) distanceGrid = findClosedKeys(List(findLocation(key,arr)),List[Door](),arr,distanceGrid)
    distanceGrid
  }

  def isKey(c:Char) =  c >= 'a' &&  c <= 'z'
  def isDoor(c:Char) =  c >= 'A' &&  c <= 'Z'
  def isStart(c: Char) = c == '@' || (c >= '1' && c<= '9')
  def isDoorKey(k: Key, d: Door) = k.c - 'a' + 'A' == d.c
  def toDoorKey(d: Door) = Key((d.c - 'A' + 'a').toChar)

  case class Key(c: Char,isStartLocation:Boolean = false){ override def toString() = c.toString}
  case class Door(c: Char){ override def toString() = c.toString}

  class Grid(grid: Map[Key,Map[Key,(Int,List[Door])]] = Map()) {

    private def getFromKeyOptions(fromKey: Key) = grid.getOrElse(fromKey, Map[Key, (Int, List[Door])]())
    private def getVisitCostConditions(fromKey: Key, toKey: Key) = getFromKeyOptions(fromKey).getOrElse(toKey, (-1, List()))

    def getNeighbours(key: Key): Set[Key] = grid.getOrElse(key, Map[Key, (Int, List[Door])]()).keySet
    def getVisitCost(fromKey: Key, toKey: Key): Int = getVisitCostConditions(fromKey,toKey)._1
    def getVisitConditions(fromKey: Key, toKey: Key): List[Door] = getVisitCostConditions(fromKey,toKey)._2

    def getNewReachOptions(visited: List[Key],newKey: Key): List[Key] = {
      val startLocations = getKeys().filter(k => k.isStartLocation)
      var result = List[Key]()
      for(startKey <- startLocations){
        val fromStartConditions = getFromKeyOptions(startKey)
        for(key <- fromStartConditions.keySet) {
          val keysNeeded = fromStartConditions.get(key).get._2.map(d => toDoorKey(d))
          if (keysNeeded.contains(newKey) && (keysNeeded diff visited).size == 1) result = result :+ key
        }
      }
      result
    }

    def addMove(fromKey: Key, toKey: Key, cost: Int, conditions: List[Door]): Grid = {
      if(getFromKeyOptions(fromKey).contains(toKey) && getFromKeyOptions(fromKey).get(toKey).get._1 < cost) return this
      new Grid(grid + (fromKey -> getFromKeyOptions(fromKey).+(toKey -> (cost, conditions))))
    }

    def addGrid(extraGrid: Grid):Grid = new Grid(grid ++ extraGrid.getGrid)
    def getGrid = grid

    def addKey(key:Key) = new Grid(grid + (key -> Map()))
    def updateKey(fromKey: Key,toKey: Key):Grid = {
      val result = grid + (toKey -> grid.get(fromKey).get)
      new Grid(result - fromKey)
    }

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

  class Buffer(){
    private var cacheMap = Map[String,(List[Key],Int)]()
    private def calcHash(loc:Key,lst: Set[Key]): String = (loc +: lst.toList.sortBy(k => k.c)).foldRight("")(_ + _)
    private def calcHash(loc:Set[Key],lst: Set[Key]): String =
      (loc.toList.sortBy(k => k.c) +: lst.toList.sortBy(k => k.c)).foldRight("")(_ + _)

    def addToCache(location:Key, toVisit: Set[Key],path: List[Key],cost: Int) = {
      cacheMap += (calcHash(location,toVisit) -> (path,cost))
    }

    def addToCache(locations:Set[Key], toVisit: Set[Key],path: List[Key],cost: Int) = {
      cacheMap += (calcHash(locations,toVisit) -> (path,cost))
    }

    def getFromCache(location: Key,toVisit: Set[Key]) : Option[(List[Key],Int)] = cacheMap.get(calcHash(location,toVisit))
    def getFromCache(locations: Set[Key],toVisit: Set[Key]) : Option[(List[Key],Int)] = cacheMap.get(calcHash(locations,toVisit))
  }
}
