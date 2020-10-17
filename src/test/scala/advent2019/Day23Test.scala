package advent2019

import org.scalatest.FunSuite

import scala.io.Source

class Day23Test extends FunSuite{
  val input = readFile("Day23.txt").toList

  private def readFile(fname: String): Seq[String] = {
    val fileHandle = getClass.getResourceAsStream("/2019/"+fname)
    if(fileHandle == null) throw new Exception("File not found: "+"/2019/"+fname)
    Source.fromInputStream(fileHandle).getLines.toSeq
  }


  test("testNic25"){
    val prog = input(0).split(",").map(s => s.toLong)

    val network = Array.ofDim[Program](50)
    var nic0 = OpcodeProcessor.processDay5OppCode(Program(prog.clone(),0),List(0))
    network(30) = OpcodeProcessor.processDay5OppCode(Program(prog,0),List(30))

    nic0  = OpcodeProcessor.processDay5OppCode(nic0,List(-1))
   // println("0 -> "+nic0.output)

    var nic25 = OpcodeProcessor.processDay5OppCode(Program(prog,0),List(0,-1))
    println("25 -> "+nic25.output)
    nic25 = OpcodeProcessor.processDay5OppCode(nic25,List(1063, 25110,2126, 25110,3189, 25110))
    println(" -> "+nic25.output)
  }
}
