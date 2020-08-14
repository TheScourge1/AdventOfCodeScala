package advent2019

object Day20 extends Day(20){
  override def testSetA = List(TestCase("Day20_testa1.txt","23"),TestCase("Day20_testa2.txt","58"))

  override def testSetB = List()

  def StringPattern= "[A-Z]{2}".r

  override def solutionA(input: List[String], params: List[String]) = {
    val gridArr = readInputArr(input)
    println(printArr(gridArr))

    val path = findShortestPath(List(("AA")),List(findLocation("AA",gridArr).head),gridArr)
    println(path)
    path._2.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }

  def findShortestPath(path:List[String],track: List[(Int, Int)], grid: Array[Array[String]]): (List[String],Int) = {
    val isPortal = StringPattern
    val distance = track.size

    var minResult = List[String]()
    var minVal = 0
    for(m <- validMoves(grid,track)){
      println(m)
      grid(m._1)(m._2) match {
        case "ZZ" =>
          return (path:+grid(m._1)(m._2),distance+1)
        case isPortal() => {
          val newPath =  findShortestPath(path:+grid(m._1)(m._2), track :+findPortalTarget(m,grid),grid)
          val newMinVal = newPath._2
          if(newMinVal > 0 && (minVal == 0 || minVal > newMinVal)) {
            minVal = newMinVal
            minResult = newPath._1
          }
        }
        case "." => return findShortestPath(path,track :+m,grid)
        case _ => throw new Exception(s"Invalid grid value found: ${grid(m._1)(m._2)} at ${m}")
      }
    }

    (minResult,minVal)
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
    val regExLeft = ".*\\Q.\\E([A-Z]{2}).*".r
    val regExRight = ".*([A-Z]{2})\\Q.\\E.*".r
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
}
