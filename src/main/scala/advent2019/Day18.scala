package advent2019

import scala.collection.mutable.ListBuffer

object Day18 extends Day(18){
  override def testSetA = List(TestCase("Day18_testa.txt","86"),TestCase("Day18_testa2.txt","132"),TestCase("Day18_testa3.txt","136"))

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val matrix = input.map(s => s.toArray).toArray
    val start = findStartLocation(matrix)
    val keySet = getAllKeys(matrix)

    var distanceMap = Map(start -> getDistance(List((start.i,start.j)),List(),matrix,Map[Key,(Int,List[Door])]()))
    for(key <- keySet) distanceMap =
      distanceMap + (key -> getDistance(List((key.i,key.j)),List(),matrix,Map[Key,(Int,List[Door])]()))
    val path = findShortestPath(List((start,0)),distanceMap)
    println(path)
    path.map(p => p._2).sum.toString
  }

  def findStartLocation(matrix: Array[Array[Char]]): Key = {
    for(i<- 0 until  matrix.size; j<- 0 until matrix(0).size)
      if(matrix(i)(j) == '@') return Key('@',i,j)

    Key('@',-1,-1)
  }

  def getAllKeys(matrix: Array[Array[Char]]): List[Key] = {
    var result = ListBuffer[Key]()
    for(i<- 0 until  matrix.size; j<- 0 until matrix(0).size)
      if(isKey(matrix(i)(j))) result += Key(matrix(i)(j),i,j)
    result.toList
  }

  def getDistance(steps: List[(Int,Int)],doors: List[Door],matrix: Array[Array[Char]],
                  targets: Map[Key,(Int,List[Door])]): Map[Key,(Int,List[Door])]= {
      val currentLoc = steps.last
      var newTargets = targets

      def validCoord(location: (Int,Int),matrix: Array[Array[Char]])=
        location._1 > 0 && location._1 < matrix.size &&
        location._2 > 0 &&  location._2 < matrix(0).size &&
        matrix(location._1)(location._2) != '#'

      var newLocations = List[(Int,Int)]()
      for(i <- -1 to 1; j <- -1 to 1) {
        val newLoc = (i + currentLoc._1, j + currentLoc._2)
        if (Math.abs(i) + Math.abs(j) == 1 && validCoord(newLoc, matrix) && !steps.contains(newLoc))
          newLocations = newLocations :+ newLoc
      }

      for(loc <- newLocations) {
        val newVal = matrix(loc._1)(loc._2)
        if(isKey(newVal)) newTargets = getDistance(steps:+loc,doors,matrix,newTargets + (newKey(loc,matrix) -> (steps.size,doors)))
        else if(isDoor(newVal)) newTargets = getDistance(steps:+loc,doors :+ newDoor(loc,matrix),matrix,newTargets)
        else  newTargets = getDistance(steps:+loc,doors,matrix,newTargets)
        }
    newTargets
  }

  def findShortestPath(path: List[(Key,Int)],distanceMap: Map[Key,Map[Key,(Int,List[Door])]]): List[(Key,Int)] = {
    if(path.size == distanceMap.keySet.size) return path
    val location = path.last._1
    val pathOptions = distanceMap.getOrElse(location,Map[Key,(Int,List[Door])]())

    var result = List[(Key,Int)]()

    for(p <- pathOptions.keySet){
      if(!path.map(v => v._1).contains(p) && hasKeys(path.map(l => l._1),pathOptions.get(p).get._2)){
        val newPath = findShortestPath(path :+ (p ,pathOptions.get(p).get._1),distanceMap)
        if(newPath.size > 0 && (result.size == 0 || result.map(l => l._2).sum > newPath.map(l => l._2).sum)) result = newPath
      }
    }
    println(result)
    result
  }

  def hasKeys(keys: List[Key],doors: List[Door]): Boolean = {
    val keyChars = keys.map(k => k.c)
    val doorChars = doors.map(k => k.c)

    for(door <- doorChars)
      if(!keyChars.contains('a' + door - 'A')) return false
    true
  }


  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }

  def isKey(c:Char) =  c >= 'a' &&  c <= 'z'
  def isDoor(c:Char) =  c >= 'A' &&  c <= 'Z'
  def newKey(l: (Int,Int),matrix: Array[Array[Char]]) = Key(matrix(l._1)(l._2),l._1,l._2)
  def newDoor(l: (Int,Int),matrix: Array[Array[Char]]) = Door(matrix(l._1)(l._2),l._1,l._2)

  case class Key(c: Char,i: Int,j:Int){}
  case class Door(c: Char,i: Int,j:Int){}
}
