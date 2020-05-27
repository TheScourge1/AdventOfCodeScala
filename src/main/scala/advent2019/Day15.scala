package advent2019

import advent2019.OpcodeProcessor.Program

object Day15 extends Day(15) {
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {

    val progVals = input(0).split(",").map(s => s.toLong)

    val prog = Program(progVals,0)
    var maxDepth = 1
    var result = -1
    while(result < 0) {
      println(s"Searching for maxDepth: ${maxDepth}")
      result = tryLocation(prog,List[(Int,Int)]((0,0)),maxDepth)
      maxDepth += 1
    }

    result.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    
    "TODO"
  }

  def tryLocation(prog: Program,locationHistory: List[(Int,Int)],maxDepth: Int): Int = {
    if(locationHistory.size >= maxDepth) return -1
    var runProgram = prog
    var validLocations = List[Int]()

    val moveList = List(1,2,3,4)
    val backTrackList = List(2,1,4,3)
    def newLocation(loc:(Int,Int),move: Int) =  move match{
        case 1 => (loc._1+1,loc._2)
        case 2 => (loc._1-1,loc._2)
        case 3 => (loc._1,loc._2+1)
        case 4 => (loc._1,loc._2-1)
      }

    for(i:Int  <- 0 until 4){
      if(!locationHistory.contains(newLocation(locationHistory.last,moveList(i)))){
        runProgram = OpcodeProcessor.processDay5OppCode(runProgram,List(moveList(i)))
        runProgram.output.last.toInt match {
          case 0 => //hit a wall. Skip to next location
          case 1 => {
            runProgram = OpcodeProcessor.processDay5OppCode(runProgram,List(backTrackList(i)))
            validLocations = validLocations :+ i
          } // backtrack and save i
          case 2 => return locationHistory.size // found the location
        }
      }
    }

    //search one deeper
    for(i <- validLocations) {
      //println(s"depth: ${locationHistory.size}: ${newLocation(locationHistory.last,moveList(i))}")
      runProgram = OpcodeProcessor.processDay5OppCode(runProgram,List(moveList(i)))
      val depthFound = tryLocation(runProgram,locationHistory :+ newLocation(locationHistory.last,moveList(i)),maxDepth)
      if(depthFound > 0) return  depthFound
      runProgram = OpcodeProcessor.processDay5OppCode(runProgram,List(backTrackList(i)))
    }

    -1
  }

}
