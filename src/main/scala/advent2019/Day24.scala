package advent2019

object Day24 extends Day(24){
  override def testSetA = List()

  override def testSetB = List()

  override def paramsB = List("200")

  override def solutionA(input: List[String], params: List[String]) = {
    var matrix = getMatrix(input)

    var stateSet = Set[Long]()
    var matrixHash = stateValue(matrix)
    while(! stateSet.contains(matrixHash)){
      stateSet += matrixHash
      matrix = executeStep(matrix)
      matrixHash = stateValue(matrix)
    }

    matrixHash.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val grid = getMatrix(input)
    val itts = params(0).toInt
    val size = grid.size

    var gridList = List(grid)
    for(i <- 0 until itts){
      gridList.foreach(g => println(printGrid(g)))
      println("____")
      if(bugCount(gridList.head) > 0) gridList = List(emptyGrid(size)) ++ gridList
      if(bugCount(gridList.last) > 0) gridList = gridList :+ emptyGrid(size)
      gridList = evolve(gridList)
    }

    gridList.map(grid => bugCount(grid)).sum.toString
  }

  def getMatrix(input: List[String]): List[List[Int]] = input.map(s =>
    s.map(c => c match{
      case '#' => 1
      case '.' => 0
      case _  => throw new Exception(("Unexpected value: ")+c)
    }).toList)

  def executeStep(matrix: List[List[Int]] ): List[List[Int]] = {
    executeStep(null,matrix,null)
  }

  def countNeigbours(up: List[List[Int]],mid: List[List[Int]],down: List[List[Int]],i:Int,j:Int): Int = {
    var result = 0

    if(up != null && down != null && i==j && i == mid.size/2) return 0

    for(row <- j-1 to j+1; col <- i-1 to i+1){
      if(Math.abs(i-col) + math.abs(j-row) == 1){
        if(row >= 0 && row < mid(0).size && col >= 0 && col < mid.size && Math.abs(i-col) + math.abs(j-row) == 1)
          result += mid(row)(col)
        else if(row == -1 && up != null)
          result += up(up.size/2-1)(up(0).size/2)
        else if(row == mid(0).size & down != null)
          result += up(up.size/2+1)(up(0).size/2)
        else if(col == -1 && up != null)
          result += up(up.size/2)(up(0).size/2-1)
        else if(col == mid.size && down != null)
          result += up(up.size/2)(up(0).size/2+1)

        if(row == mid.size/2 && row == col && down != null && up != null){ // count down
          if(j == mid.size/2-1) result += down(0).sum
          if(j == mid.size/2+1) result += down(down.size-1).sum

          if(i == mid.size/2-1) result += down.map(_(0)).sum
          if(i == mid.size/2+1) result += down.map(_(down.size-1)).sum
        }
      }
    }
    result
  }

  def stateValue(matrix: List[List[Int]]): Long = {
    var result = 0L
    val lst = matrix.flatten
    for(i <- 0 until lst.size) result += Math.pow(2,i).toLong * lst(i)
    result
  }

  def bugCount(matrix: List[List[Int]]): Int = matrix.flatten.count(_ > 0)

  def emptyGrid(size:Int): List[List[Int]] =
    List.fill[List[Int]](size)(List.fill[Int](size)(0))

  def evolve(gridList: List[List[List[Int]]]): List[List[List[Int]]] = {
    var result = List[List[List[Int]]]()

    for(i <- 0 until gridList.size)
      result = result :+ executeStep(gridList(math.max(0,i-1)),gridList(i),gridList(math.min(gridList.size-1,i+1)))

    result
  }

  def executeStep(up: List[List[Int]],mid: List[List[Int]],down:List[List[Int]]) : List[List[Int]] = {
    val tempArr = Array.ofDim[Int](mid(0).size,mid.size)

    for(i <- 0 until mid(0).size; j<- 0 until mid.size){
      val neighbours = countNeigbours(up,mid,down,i,j)
      tempArr(j)(i) =  mid(j)(i) match {
        case 1 => if(neighbours  == 1) 1 else 0
        case 0 => if(neighbours > 0 && neighbours < 3) 1 else 0
      }
    }

    tempArr.toList.map(a => a.toList)
  }

  def printGrid(grid: List[List[Int]]): String = {
    grid.map(lst => lst.fold("")(_.toString+_.toString).toString).fold("")(_+"\n"+_)
  }

}
