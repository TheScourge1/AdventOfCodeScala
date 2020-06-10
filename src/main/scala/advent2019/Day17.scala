package advent2019

import advent2019.OpcodeProcessor.Program

import scala.collection.mutable.ListBuffer

object Day17 extends Day(17){
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val prog = input(0).split(",").map(s => s.toLong)
    val res = OpcodeProcessor.processDay5OppCode(Program(prog,0),List())

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
    val prog = input(0).split(",").map(s => s.toLong)

    var res = OpcodeProcessor.processDay5OppCode(Program(prog,0),List())
    val route = getRoute(processOutput(res.output))
    val subRoutes = splitRoute(route)

    var progInput = List[Int]()
    progInput = progInput ++ convertToIntCodes(subRoutes._2)
    for( l<- subRoutes._1)
      progInput = progInput ++ convertToIntCodes(l)
    progInput = progInput :+ 'n'.toInt
    progInput = progInput :+ 10

    prog(0) = 2
    res = OpcodeProcessor.processDay5OppCode(Program(prog,0),progInput)
    res.output.last
  }

  def convertToIntCodes(lst: List[String]): List[Int] = {
    var result = List[Int]()
    for(s <- lst){
      s.chars().forEach(c => result=result :+ c.toInt)
      result=result :+ 44
    }
    if(result.size > 0)  result.slice(0,result.size-1) :+ 10
    else result
  }

  def splitRoute(input: List[String]): (List[List[String]],List[String]) = {
     for(ia <- 2 to 20 by 2;ib <-2 to 20 by 2; ic <- 2 to 20 by 2){
      var a,b,c = List[String]()
      a = input.slice(0,ia)
      var ir = 0
      var invalid = false
      var routes = List[String]()
      while(ir<input.size && !invalid){
        if(input.slice(ir,ir+a.size).equals(a)) {ir +=a.size; routes = routes :+ "A"}
        else {
          if (b.size == 0) b = input.slice(ir, ir + ib)
          if (input.slice(ir, ir + b.size).equals(b)) {ir += b.size; routes = routes :+ "B"}
          else {
            if (c.size == 0) c = input.slice(ir, ir + ic)
            if(input.slice(ir,ir+c.size).equals(c)) {ir +=c.size; routes = routes :+ "C"}
            else invalid = true
          }
        }
      }
      if(!invalid) return (List(a,b,c),routes)
    }

    (List(),List())
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
      l._1 >= 0 && l._2 >= 0 && l._1 < input.size && l._2 < input(0).size && input(l._1)(l._2).equals('#')

  def stepForward(l:(Int,Int,Int)) = l._3 match{
    case 0 => (l._1-1,l._2,0)
    case 3 => (l._1,l._2+1,3)
    case 6 => (l._1+1,l._2,6)
    case 9 => (l._1,l._2-1,9)
    case _ => (-1,-1,-1)
  }

  def stepRight(l:(Int,Int,Int))= stepForward((l._1,l._2,(l._3+3)%12))
  def stepLeft(l:(Int,Int,Int))= stepForward((l._1,l._2,(12+l._3-3)%12))

  def printOutput(code: List[List[Char]])=
    for(l <- code) println(l.map(_.toString).fold("")(_+_))
}
