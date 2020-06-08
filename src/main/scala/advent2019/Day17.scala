package advent2019

import advent2019.OpcodeProcessor.Program

import scala.collection.mutable.ListBuffer

object Day17 extends Day(17){
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val prog = input(0).split(",").map(s => s.toLong)
    val res = OpcodeProcessor.processDay5OppCode(Program(prog,0),List())
    printOutput(processOutput(res.output))
    println(getRoute(processOutput(res.output)))
    getIntersections(processOutput(res.output)).map(t => t._1*t._2).sum.toString
  }



  def processOutput(code: List[String]): List[List[Char]] =
    code.filter(!_.equals("10")).map(s => s.toInt.toChar).grouped(code.indexOf("10")).toList

  def getIntersections(input: List[List[Char]]): List[(Int,Int)] = {
    val res = for{
      i <- 0 until input.size
      j <- 0 until input(0).size
      if(isCrossing(input,(i,j)))
     } yield (i,j)

    res.toList
  }

  def isCrossing(input: List[List[Char]],l: (Int,Int)): Boolean =
    input(l._1)(l._2).equals('#') && l._1 != 0 && l._2!= 0 &&
      l._1 != input.size-1 && l._2!= input(0).size &&
      input(l._1-1)(l._2).equals('#')&& input(l._1+1)(l._2).equals('#') &&
      input(l._1)(l._2-1).equals('#')&& input(l._1)(l._2+1).equals('#')


  override def solutionB(input: List[String], params: List[String]) = {
    var prog = input(0).split(",").map(s => s.toLong)
    prog(0) = 2
    val res = OpcodeProcessor.processDay5OppCode(Program(prog,0),List())

    "TODO"
  }

  def getRoute(input: List[List[Char]]): List[String] = {
    val result = ListBuffer[String]()
    var s = getStartLocation(input)
    var steps = 0
    while(s != (-1,-1,-1)){
      val newPos: (Int,Int,Int) =
        if(isValidLocation(input,stepForward(s))) {
          steps+=1;
          stepForward(s)
        }
        else if(isValidLocation(input,stepRight(s))) {
          if(steps > 0) result += steps.toString
          steps = 1
          result +="R";
          stepRight(s)
        }
        else if(isValidLocation(input,stepLeft(s))) {
          if(steps > 0) result += steps.toString
          steps = 1
          result +="L";
          stepLeft(s)
        }
      else{
          if(steps > 0) result += steps.toString
          steps = 0
          (-1,-1,-1)
        }
      s = newPos
    }

    result.toList
  }

  /**
   *
   * @param input
   * @return return coordinates (i,j) plus facing direction as (0,3,6,9)
   */
  def getStartLocation(input: List[List[Char]]): (Int,Int,Int) = {
    for(i <- 0 until input.size ; j <- 0 until input(0).size)
      input(i)(j) match {
        case '^' => return (i,j,0)
        case '>' => return (i,j,3)
        case 'v' => return (i,j,6)
        case '<' => return (i,j,9)
        case _ =>
      }
    (-1,-1,-1)
  }

  def isValidLocation(input: List[List[Char]],l: (Int,Int,Int)): Boolean =
      l._1 >= 0 && l._2 >= 0 &&
      l._1 < input.size && l._2 < input(0).size &&
        input(l._1)(l._2).equals('#')

  def stepForward(l:(Int,Int,Int)) = l._3 match{
    case 0 => (l._1-1,l._2,0)
    case 3 => (l._1,l._2+1,3)
    case 6 => (l._1+1,l._2,6)
    case 9 => (l._1,l._2-1,9)
    case _ => (-1,-1,-1)
  }

  def stepRight(l:(Int,Int,Int))= stepForward((l._1,l._2,(l._3+3)%12))
  def stepLeft(l:(Int,Int,Int))= stepForward((l._1,l._2,(12+l._3-3)%12))

  def printOutput(code: List[List[Char]])={
    for(l <- code) println(l.map(_.toString).fold("")(_+_))
  }
}
