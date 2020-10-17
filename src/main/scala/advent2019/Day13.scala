package advent2019

object Day13 extends Day(13){
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val progVals = input(0).split(",").map(s => s.toLong)

    val result = OpcodeProcessor.processDay5OppCode(Program(progVals,0),List())
    val tiles = for(i <- 0 until result.output.size/3) yield result.output(2+i*3)
    tiles.filter(p => p =="2").size.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val progVals = input(0).split(",").map(s => s.toLong)
    var prog = Program(progVals,0)
    prog.write(0,2L,0)
    prog = OpcodeProcessor.processDay5OppCode(prog,List())
    var screen = Map[(Int,Int),Tile]()
    while(!prog.isFinished){
      screen = updateScreen(screen, prog.output)

      val ball = screen.values.filter(p => p.value == 4).head
      val paddle = screen.values.filter(p => p.value == 3).head

      val input =
        if(ball.x < paddle.x) -1
        else if (ball.x > paddle.x) 1
        else 0

      prog = prog.clearOutput()
      prog = OpcodeProcessor.processDay5OppCode(prog,List(input))
    }

    screen = updateScreen(screen, prog.output)
    screen.values.toList.filter(p => p.x == -1).head.value.toString
  }

  def updateScreen(screen: Map[(Int,Int),Tile],output: List[String]): Map[(Int,Int),Tile] = {
    var result = screen
    for(i <- 0 until output.size/3)
      result = result + ((output(i*3).toInt,output(1+i*3).toInt) -> Tile(output(i*3).toInt,output(1+i*3).toInt,output(2+i*3).toInt))

    result
  }

  case class Tile(x:Int,y:Int,value:Int)
}
