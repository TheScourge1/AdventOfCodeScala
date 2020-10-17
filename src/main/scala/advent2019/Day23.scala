package advent2019


object Day23 extends Day(23){
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]):String = {
    val prog = input(0).split(",").map(s => s.toLong)
    val ComputerCount = 50
    var networkState = initNetwork(ComputerCount,prog)

    var address = -1
    while(true){
      address = (address + 1)%ComputerCount
      networkState = executeProcess(networkState,address)
      if(networkState.pendingInstructions.contains(255)) return networkState.pendingInstructions(255)(1).toString
    }
    "ERROR"
  }

  override def solutionB(input: List[String], params: List[String]):String = {
    val prog = input(0).split(",").map(s => s.toLong)
    val ComputerCount = 50
    var networkState = initNetwork(ComputerCount,prog)

    var yValueHistory = Set[Long]()

    while(true){
      for(address <- 0 until ComputerCount) {
        networkState = executeProcess(networkState,address)

        if(!networkState.hasPendingInstructions){
          val natInstruction = networkState.pendingInstructions(255)
          if(yValueHistory.contains(natInstruction.last))
            return natInstruction.last.toString
          else{
            networkState = Network(networkState.network,
              networkState.pendingInstructions + (0 -> natInstruction.slice(natInstruction.size-2,natInstruction.size)))
            yValueHistory = yValueHistory + natInstruction.last
          }
        }
      }
    }

    "ERROR"
  }

  def initNetwork(networkSize:Int,prog:Array[Long]): Network = {

  import advent2019.Day23.executeProcess

  var network = List[Program]()
    var instructionMap = Map[Int,List[Long]]()

    for(i <- 0 until networkSize){
      val newProg = OpcodeProcessor.processDay5OppCode(Program(prog.clone(),0),List(i))
      newProg.output.map(_.toLong).grouped(3).foreach(g =>
        instructionMap += g(0).toInt -> (instructionMap.getOrElse(g(0).toInt,List()) ++ g.slice(1,3))
      )
      network = network :+ newProg.clearOutput()
    }

    Network(network,instructionMap)
  }

  def executeProcess(network:Network,address:Int): Network = {
    val instructions = network.pendingInstructions.getOrElse(address,List(-1L))
    var newInstructions = network.pendingInstructions.updated(address, List.empty)

    var newCPU = OpcodeProcessor.processDay5OppCode(network.network(address), instructions)

    for (g <- newCPU.output.map(_.toLong).grouped(3))
      newInstructions += (g(0).toInt -> (newInstructions.getOrElse(g(0).toInt,List.empty) ++ g.slice(1, 3)))
    newCPU = newCPU.clearOutput()

    Network(network.network.updated(address,newCPU),newInstructions)
  }



  case class Network(network:List[Program],pendingInstructions :Map[Int,List[Long]]){

    def hasPendingInstructions: Boolean = pendingInstructions.count(p => p._1 != 255 && p._2.size > 0) > 0

  }
}
