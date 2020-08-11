package advent2019

import advent2019.OpcodeProcessor.Program

object Day19 extends Day(19){
  override def testSetA = List(TestCase("Day19.txt","5",List("10x10")))

  override def paramsA = List("50x50")

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val prog = input(0).split(",").map(s => s.toLong)
    val gridSizeXY = params(0).split("x").map(a => a.toInt)
    val scanResult = Array.ofDim[Int](gridSizeXY(0),gridSizeXY(1))
    for(x <- 0 until gridSizeXY(0);y <- 0 until gridSizeXY(1))
      scanResult(y)(x) = valueAt(x,y,prog)

    scanResult.flatten.count(p => p == 1).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val prog = input(0).split(",").map(s => s.toLong)
    val SquareSize = 100

    var x,y = SquareSize
    while(!validSquareUpRight(x,y,SquareSize,prog)){
      y = y+1
      x = firstValidX(x,y,prog)
    }

    (x*10000 + y-SquareSize+1).toString
  }

  def validSquareUpRight(x:Int,y:Int,squareSize:Int, prog:Array[Long]): Boolean = {
    if(x< squareSize || y < squareSize) return false

    if(valueAt(x,y,prog) == 1 && valueAt(x+squareSize-1,y,prog) == 1
      && valueAt(x,y-squareSize+1,prog) == 1 && valueAt(x+squareSize-1,y-squareSize+1,prog) == 1) true
    else false
  }

  def firstValidX(x:Int,y:Int,prog:Array[Long]): Int = {
    var newX = x
    if(y<=4) return x
    while(valueAt(newX,y,prog) != 1) newX +=1
    newX
  }

  def valueAt(x:Int,y:Int,prog:Array[Long]) = OpcodeProcessor.processDay5OppCode(Program(prog,0),List(x,y)).output.head.toInt

  def printArr(arr: Array[Array[Int]]): String = {
    var result = new StringBuilder()
    for(i <- 0 until arr.size) {
      for(j <- 0 until arr(0).size) result =  result.append(arr(i)(j).toString + " ")
      result.append("\n")
    }
    result.toString
  }
}
