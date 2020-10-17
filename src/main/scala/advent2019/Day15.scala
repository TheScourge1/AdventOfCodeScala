package advent2019

import advent2019.Program

object Day15 extends Day(15) {
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val progVals = input(0).split(",").map(s => s.toLong)
    (findOxigen(Program(progVals,0),List[(Int,Int)]((0,0)))._1.size-1).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val progVals = input(0).split(",").map(s => s.toLong)
    val oxigenLocation = findOxigen(Program(progVals,0),List[(Int,Int)]((0,0)))

    (findMaxSteps(oxigenLocation._2,List(oxigenLocation._1.last))-1).toString
  }

  def findOxigen(inputProgram: Program, hist: List[(Int,Int)]): (List[(Int,Int)],Program) = {
    var prog = inputProgram
    var validSteps = List[Int]()

    for(step <- Moves.moves) {
      if(!Moves.visited(hist,step)){
        prog = OpcodeProcessor.processDay5OppCode(prog,List(step))
        prog.output.last.toInt match {
          case 0 => //hit a wall. Skip to next location
          case 1 => {
            prog = OpcodeProcessor.processDay5OppCode(prog,List(Moves.backMove(step)))
            validSteps = validSteps :+ step
          } // backtrack and save i
          case 2 => return (Moves.addLocation(hist,step),prog) // found the location
        }
      }
    }

    //search one deeper
    for(step <- validSteps) {
      prog = OpcodeProcessor.processDay5OppCode(prog,List(step))
      val result = findOxigen(prog,Moves.addLocation(hist,step))
      if(result._1.size > 0) return result
      else prog = OpcodeProcessor.processDay5OppCode(prog,List(Moves.backMove(step)))
    }
    (List(),prog)
  }

  def findMaxSteps(inputProgram: Program, hist: List[(Int,Int)]): Int = {
    var prog = inputProgram
    val maxSteps = for(step <- Moves.moves) yield {
      var res = 0
      if(!Moves.visited(hist,step)){
        prog = OpcodeProcessor.processDay5OppCode(prog,List(step))
        if (prog.output.last.toInt == 1) {
            res = findMaxSteps(prog,Moves.addLocation(hist,step))
            prog = OpcodeProcessor.processDay5OppCode(prog,List(Moves.backMove(step)))
        }
      }
      res
    }
   maxSteps.max +1
  }


  object Moves{
    val moves = List(1,2,3,4)

    def backMove(step: Int) = step match {
      case 1 => 2
      case 2 => 1
      case 3 => 4
      case 4 => 3
    }

    def newLocation(l:(Int,Int),move: Int) =  move match{
      case 1 => (l._1+1,l._2)
      case 2 => (l._1-1,l._2)
      case 3 => (l._1,l._2+1)
      case 4 => (l._1,l._2-1)
    }

    def addLocation(hist: List[(Int,Int)], step: Int): List[(Int,Int)] =
      hist :+ Moves.newLocation(hist.last,step)

    def visited(hist: List[(Int,Int)], step: Int): Boolean =
      hist.contains(Moves.newLocation(hist.last,step))
  }

}
