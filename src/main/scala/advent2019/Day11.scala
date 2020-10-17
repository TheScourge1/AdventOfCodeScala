package advent2019

object Day11 extends Day(11) {
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val progVals = input(0).split(",").map(s => s.toLong)

    var visitMap = Map[(Int,Int),Int]()
    var loc = (1000,1000,"N")
    var program = Program(progVals,0)

    while(!program.isFinished){
      program = OpcodeProcessor.processDay5OppCode(program, List(visitMap.getOrElse((loc._1,loc._2),0)))
      visitMap = visitMap + ((loc._1,loc._2) -> program.output(0).toInt)
      loc = executeStep(loc,program.output(1).toInt)
      program = program.clearOutput()
    }

    visitMap.keySet.size.toString
  }


  override def solutionB(input: List[String], params: List[String]) = {
    val progVals = input(0).split(",").map(s => s.toLong)

    var visitMap = Map[(Int,Int),Int]()
    var loc = (100,100,"N")
    var program = Program(progVals,0)

    while(!program.isFinished){
      program = OpcodeProcessor.processDay5OppCode(program, List(visitMap.getOrElse((loc._1,loc._2),1)))
      visitMap = visitMap + ((loc._1,loc._2) -> program.output(0).toInt)
      loc = executeStep(loc,program.output(1).toInt)
      program = program.clearOutput()
    }

    val maxX = visitMap.keySet.map(s =>s._1).max
    val maxY = visitMap.keySet.map(s =>s._2).max
    val minX = visitMap.keySet.map(s =>s._1).min
    val minY = visitMap.keySet.map(s =>s._2).min
    val matrix = Array.ofDim[String](maxX-minX+1,maxY-minY+1)
    for(key <- visitMap.keySet) matrix(key._1-minX)(key._2-minY) = if(visitMap.getOrElse(key,0)==0) " " else "#"
    for(x <- 0 until matrix.size){
      for(y <- 0 until matrix(0).size) print(matrix(x)(y))
      println
    }
    "DONE"
  }


  def executeStep(location: (Int,Int,String),direction: Int): (Int,Int,String) ={
    val DIRS = List("N","O","S","W")

    val newDir = direction match{
      case 0 => DIRS((4+DIRS.indexOf(location._3) - 1)%4)
      case 1 => DIRS((DIRS.indexOf(location._3) + 1)%4)
    }

    newDir match{
      case "N" => (location._1,location._2+1,newDir)
      case "O" => (location._1+1,location._2,newDir)
      case "S" => (location._1,location._2-1,newDir)
      case "W" => (location._1-1,location._2,newDir)
    }
  }
}
