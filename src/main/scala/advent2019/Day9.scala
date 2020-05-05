package advent2019

import advent2019.OpcodeProcessor.Program

object Day9 extends Day(9){
  override def testSetA = List(TestCase("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
    "109"),
    TestCase("1102,34915192,34915192,7,4,7,99,0","1219070632396864"),
    TestCase("104,1125899906842624,99","1125899906842624"))

  override def paramsA: List[String] = List("1")

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val program = input(0).split(",").map(s => s.toLong)

    OpcodeProcessor.processDay5OppCode(Program(program,0),List(1)).output.toString

  }

  override def solutionB(input: List[String], params: List[String]) = {
    "TO DO"
  }
}
