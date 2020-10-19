package advent2019

object Day24 extends Day(24){
  override def testSetA = List()

  override def testSetB = List()

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

    "TODO"
  }

  def getMatrix(input: List[String]): List[List[Int]] = input.map(s =>
    s.map(c => c match{
      case '#' => 1
      case '.' => 0
      case _  => throw new Exception(("Unexpected value: ")+c)
    }).toList)

  def executeStep(matrix: List[List[Int]] ): List[List[Int]] = {
    val tempArr = Array.ofDim[Int](matrix(0).size,matrix.size)
    for(i <- 0 until matrix(0).size; j<- 0 until matrix.size){
      val neighbours = countNeigbours(matrix,i,j)
      tempArr(j)(i) =  matrix(j)(i) match {
        case 1 => if(neighbours  == 1) 1 else 0
        case 0 => if(neighbours > 0 && neighbours < 3) 1 else 0
      }
    }

    tempArr.toList.map(a => a.toList)
  }

  def countNeigbours(matrix: List[List[Int]],i:Int,j:Int): Int = {
    var result = 0
    for(row <- j-1 to j+1; col <- i-1 to i+1){
      if(row >= 0 && row < matrix(0).size && col >= 0 && col < matrix.size && Math.abs(i-col) + math.abs(j-row) == 1)
        result += matrix(row)(col)
    }
    result
  }

  def stateValue(matrix: List[List[Int]]): Long = {
    var result = 0L
    val lst = matrix.flatten
    for(i <- 0 until lst.size) result += Math.pow(2,i).toLong * lst(i)
    result
  }


  def calcBioRating(matrix: List[List[Int]]): Long = {
    0
  }
}
