package advent2019

import advent2019.Day24.{countNeigbours, executeStep, getMatrix, stateValue}
import org.scalatest.FunSuite

import scala.io.Source

class Day24Test extends FunSuite{
  val input = readFile("Day24Test.txt").toList

  private def readFile(fname: String): Seq[String] = {
    val fileHandle = getClass.getResourceAsStream("/2019/"+fname)
    if(fileHandle == null) throw new Exception("File not found: "+"/2019/"+fname)
    Source.fromInputStream(fileHandle).getLines.toSeq
  }

  test("countNeighbours"){
    val matrix = getMatrix(input)

    assert(countNeigbours(null,matrix,null,0,0) ==1)
    assert(countNeigbours(null,matrix,null,0,1) ==1)
    assert(countNeigbours(null,matrix,null,2,0) ==0)
    assert(countNeigbours(null,matrix,null,3,2) ==2)
  }

  test("ExecuteStep"){
    val matrix = getMatrix(input)
    val stepMatrix = executeStep(matrix)
    println(matrix)
    println(stepMatrix)

    assert(stepMatrix(0)(0) == 1)
    assert(stepMatrix(0)(1) == 0)
    assert(stepMatrix(1)(0) == 1)
    assert(stepMatrix(1)(1) == 1)
  }

  test("testBiodiversity"){
    var matrix = getMatrix(input)

    var stateSet = Set[Long]()
    var matrixHash = stateValue(matrix)
    while(! stateSet.contains(matrixHash)){
      stateSet += matrixHash
      matrix = executeStep(matrix)
      matrixHash = stateValue(matrix)
    }

    println(matrix)
    assert(stateValue(matrix) == 2129920)
  }

}
