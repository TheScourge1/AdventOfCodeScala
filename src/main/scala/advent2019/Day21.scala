package advent2019

import advent2019.OpcodeProcessor.Program

object Day21 extends Day(21){
  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val prog = input(0).split(",").map(s => s.toLong)

    val springScript = "NOT A T\nNOT B J\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n"

    val result = OpcodeProcessor.processDay5OppCode(Program(prog,0),toIntCode(springScript))
    if(toResultString(result.output).last.toInt > 200)  result.getParam(1).toString
    else {println("Error found: "+toResultString(result.output));"ERROR"}
  }

  override def solutionB(input: List[String], params: List[String]) = {


    "TODO"
  }

  def toIntCode(input:String): List[Int] = input.toCharArray.map(c => c.toInt).toList

  def toResultString(input: List[String]) = input.map(s=>s.toInt.toChar).foldLeft("")(_+_)
}
