package advent2019

import scala.collection.mutable.ListBuffer

object Day20 extends Day(20){
  override def testSetA = List(TestCase("Day20_testa1.txt","23"),TestCase("Day20_testa2.txt","58"))

  override def testSetB = List(TestCase("Day20_testa1.txt","26"),TestCase("Day20_testb1.txt","396"))

  def StringPattern= "[A-Z]{2}".r

  override def solutionA(input: List[String], params: List[String]) = {
    val gridArr = readInputArr(input)
    val path = findShortestPath(List(("AA")),List(findLocation("AA",gridArr).head),gridArr)
    path._2.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val gridArr = readInputArr(input)
    val portalEnds = getPortalEnds(gridArr)
    var reachablePortalsEnds = Map[PortalEnd,List[(PortalEnd,Int)]]()
    for(pe <- portalEnds) reachablePortalsEnds = reachablePortalsEnds + (pe -> getPortalEndsInReach(pe,gridArr))
    val result = findShortestPathB(portalEnds.filter(p => p.name == "AA"),reachablePortalsEnds)
    result._2.toString
  }

  def findShortestPath(path:List[String],track: List[(Int, Int)], grid: Array[Array[String]]): (List[String],Int) = {
    val isPortal = StringPattern
    val distance = track.size

    var minResult = List[String]()
    var minVal = 0
    for(m <- validMoves(grid,track)){
      var newPath = (List[String](),0)
      grid(m._1)(m._2) match {
        case "ZZ" => return (path:+grid(m._1)(m._2)+track.size,distance)
        case isPortal() => newPath =  findShortestPath(path:+(grid(m._1)(m._2)+track.size),
          track :+m :+ findPortalTarget(m,grid),grid)
        case "." => newPath = findShortestPath(path,track :+m,grid)
        case _ => throw new Exception(s"Invalid grid value found: ${grid(m._1)(m._2)} at ${m}")
      }
      val newMinVal = newPath._2
      if(newMinVal > 0 && (minVal ==  0 || minVal > newMinVal)) {
        minVal = newMinVal
        minResult = newPath._1
      }
    }

    (minResult,minVal)
  }

  def getPortalEnds(grid: Array[Array[String]]): List[PortalEnd] = {
    var result = ListBuffer[PortalEnd]()
    for(i <- 0 until grid.size;j <- 0 until grid(0).size)
      if(grid(i)(j).matches(StringPattern.regex)) {
        val newPe = PortalEnd(grid(i)(j), i, j, grid)
        result += newPe
        if (newPe.name != "AA" && newPe.name != "ZZ") result += newPe.oppositePortalEnd()
      }
    result.toList
  }

  def getPortalEndsInReach(pe: PortalEnd , grid: Array[Array[String]]): List[(PortalEnd,Int)] = {
    def doStep(path: List[(Int,Int)], grid: Array[Array[String]]): List[(PortalEnd,Int)] = {
      var result = List[(PortalEnd,Int)]()
      val isPortal = StringPattern

      for(m <- validMoves(grid,path)){
        grid(m._1)(m._2) match {
          case isPortal() => result = result :+ (PortalEnd(grid(m._1)(m._2),m._1,m._2,grid),path.size)
          case "." => result = result ++ doStep(path :+ m,grid)
          case _ =>
        }
      }
      result
    }
    doStep(List((pe.x,pe.y)),grid)
  }

  def findShortestPathB(path: List[PortalEnd], map: Map[PortalEnd,List[(PortalEnd,Int)]] ):(List[PortalEnd] ,Int)= {

    val stack = VisitOptionsStack()
    stack.pushPath(path,0)
    while(true){
      val nextOption = stack.popShortestPathOption()
      val length = nextOption._1
      val path = nextOption._2
      val location = path.last

      for(nextPortal <- map(location)){
        if(depth(path) == 0 && nextPortal._1.name == "ZZ")
          return (path :+ nextPortal._1,length+nextPortal._2)
        else if(depth(path) <= 0 && nextPortal._1.name != "ZZ" && nextPortal._1.name != "AA")
          stack.pushPath(path :+nextPortal._1.oppositePortalEnd,length+nextPortal._2+1)
      }
    }

    (List(),-1)
  }

  def depth(path: List[PortalEnd]): Int = {
    path.map(pe => if(pe.isOuterEdge) -1 else 1).sum +1
  }

  def validMoves(grid: Array[Array[String]],track: List[(Int,Int)]) : List[(Int,Int)] = {
    val location = track.last
    var result = List[(Int,Int)]()
    for(i <- location._1-1 to location._1+1; j <- location._2 -1 to location._2 +1){
      if(Math.abs(i-location._1) + Math.abs(j-location._2) == 1
       && i >= 0 && i < grid.size && j >=0 && j < grid(0).size
      && !track.contains((i,j))){
        if(grid(i)(j) == "." || grid(i)(j).matches(StringPattern.regex)) result = result :+ (i,j)
        else if(grid(i)(j) != "#" && grid(i)(j) !=" ")
          throw new Exception(s"Invalid grid value found: ${grid(i)(j)} at (${i},${j})")
      }
    }

    result
  }

  def onOuterEdge(m: (Int,Int),grid: Array[Array[String]]) =
    m._1 == 0 || m._1 == grid.size-1 || m._2 == 0 || m._2 == grid(0).size-1

  def findLocation(token: String, grid: Array[Array[String]]): List[(Int,Int)] = {
    var result = List[(Int,Int)]()
    for(i <- 0 until grid.size)
      for(j <- 0 until grid(0).size)
        if(grid(i)(j).equals(token)) result = result :+ (i,j)
    result
  }

  def findPortalTarget(location: (Int,Int),grid: Array[Array[String]]) =
    findLocation(grid(location._1)(location._2),grid).filterNot(p => p==location).head

  def readInputArr(input: List[String]): Array[Array[String]] = {
    val BorderSize = 2

    var result = createGridMatrix(input,BorderSize)
    result = insertHorizontalPortals(result,input,BorderSize)
    result = insertVertialPortals(result,input,BorderSize)

    result
  }

  def createGridMatrix(input: List[String],borderSize: Int): Array[Array[String]] = {
    input.slice(borderSize,input.size-borderSize)
      .map(s => s.substring(borderSize,s.size))
      .map(s => s.toCharArray.map(c => if(c >= 'A' && c<= 'Z') ' ' else c)
                             .map(c => c.toString))
      .toArray
  }

  def insertHorizontalPortals(grid: Array[Array[String]],input: List[String],borderSize: Int):Array[Array[String]] = {
    val regExLeft = "\\Q.\\E([A-Z]{2})".r
    val regExRight = "([A-Z]{2})\\Q.\\E".r
    for(i <- 0 until input.size){
      for(str <- regExLeft.findAllMatchIn(input(i)).map(f => f.group(1)))
        grid(i-borderSize)(input(i).indexOf(str)-borderSize-1) = str
      for(str <- regExRight.findAllMatchIn(input(i)).map(f => f.group(1)))
        grid(i-borderSize)(input(i).indexOf(str)) = str
    }
    grid
  }

  def insertVertialPortals(grid: Array[Array[String]],input: List[String],borderSize: Int): Array[Array[String]] = {
    val rotatedInput = rotateInput(input)
    var rotatedGrid = createGridMatrix(rotatedInput,borderSize)
    rotatedGrid = insertHorizontalPortals(rotatedGrid,rotatedInput,borderSize)

    for(i <- 0 until rotatedGrid.size)
      for(j <- 0 until rotatedGrid(0).size)
          if(rotatedGrid(i)(j).matches(StringPattern.regex)) grid(j)(i) = rotatedGrid(i)(j)

    grid
  }

  def rotateInput(input: List[String]): List[String] = {
    if(input.size == 0) return List()
    val maxWith = input.map(s => s.size).max

    val result = Array.fill[String](maxWith)("")
    for(s <- input)
      for(i <- 0 until maxWith)
        result(i) = result(i)+ (if(i >= s.size) " " else s.charAt(i).toString)

    result.toList
  }

  def printArr(arr: Array[Array[String]]): String = {
    var res = new StringBuilder
    for(i <- 0 until arr.size){
      for(j <- 0 until arr(0).size) {
        res = res.append(arr(i)(j))
        if(arr(i)(j).size == 1) res = res.append(" ")
      }
      res = res.append("\n")
    }
    res.toString
  }

  case class PortalEnd(name:String,x:Int,y:Int,grid:Array[Array[String]]) {
    override def toString() = name + (if(isOuterEdge) "-1" else "1")
    def oppositePortalEnd():PortalEnd = {
      val coords = findPortalTarget((x,y),grid)
      PortalEnd(name,coords._1,coords._2,grid)
    }

    def isOuterEdge: Boolean = onOuterEdge((x,y),grid)
  }

  case class VisitOptionsStack() {
    private var map = Map[Int,List[List[PortalEnd]]]()

    def popShortestPathOption():(Int,List[PortalEnd]) = {
      val length = map.keys.min
      val nextPath = map(length).head
      if(map(length).size == 1)
        map = map - length
      else map = map + (length -> map(length).slice(1,nextPath.size))
      (length,nextPath)
    }

    def pushPath(path: List[PortalEnd],length:Int) = {
      val newPathList = map.getOrElse(length,List[List[PortalEnd]]()) :+ path
      map = map + (length -> newPathList)
    }
  }
}
