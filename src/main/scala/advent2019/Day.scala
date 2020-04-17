package advent2019

import scala.io.Source

abstract class Day(day: Int) extends App {

  case class TestCase(input: String,expectedResult: String,params :List[String]= List[String]()) {
    def eval(f: (String,List[String]) => String): Either[String,Boolean] = {
      val res = f(input,params)
      if(res.equalsIgnoreCase(expectedResult)) return Right(true)
      else return Left("Expected: "+expectedResult + " but found "+res)
    }
  }

  // Define a set of testCases to test the result
  def testSetA: List[TestCase]
  def testSetB: List[TestCase]

  //Define the input value here.
  def inputA: String = "Day"+day+".txt"
  def inputB: String = "Day"+day+".txt"

  def paramsA: List[String] = List()
  def paramsB: List[String] = List()

  // Write the solution to the excercise here
  def solutionA(input: List[String],params: List[String] = List()): String
  def solutionB(input: List[String],params: List[String] = List()): String

  def solutionA(input: String,params: List[String]): String = solutionA(fileOrData(input),params)
  def solutionB(input: String,params: List[String]): String = solutionB(fileOrData(input),params)

  private def fileOrData(input: String): List[String] = {
    if(input.endsWith(".txt")) readFile(input).toList
    else input.split("\n").toList
  }

  private def runTests(testList: List[TestCase],solution: (String, List[String]) => String): Either[String,Boolean] = {
    if(testList == null || testList.size == 0) return Left("Missing testcases")
    testList.foldLeft(Right(true).asInstanceOf[Either[String,Boolean]])((res : Either[String,Boolean],test: TestCase) => res match {
      case Right(true) => test.eval(solution)
      case Left(x) => Left(x)
    })
  }

  private def readFile(fname: String): Seq[String] = {
    val fileHandle = getClass.getResourceAsStream("/2019/"+fname)
    if(fileHandle == null) throw new Exception("File not found: "+"/2019/"+fname)
    return Source.fromInputStream(fileHandle).getLines.toSeq
  }

  override def main(args: Array[String]) = {
    println(s"Run test ${day}")
    if(testSetA == null) println("Missing testcases for solution A")
    else runTests(testSetA,solutionA) match {
      case Left(x) => println(s"Error in test A: ${x}")
      case Right(true) => println(s"Success for A!!  ${testSetA.size} tests executed")
    }

    if(inputA == null) println("Missing input for solution A")
    else println("Day 1 Solution A: "+solutionA(inputA,paramsA))

    if(testSetB == null) println("Missing testcases For solution B")
    else runTests(testSetB,solutionB) match {
      case Left(x) => println(s"Error in test B: ${x}")
      case Right(true) => println(s"Success for B!!  ${testSetB.size} tests executed")
    }

    if(inputB == null) println("Missing input for solution B")
    else println(s"Day ${day} Solution B: "+solutionB(inputB,paramsB))
  }

}
