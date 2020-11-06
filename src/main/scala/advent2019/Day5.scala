package advent2019

object Day5 extends Day(5){
  override def testSetA = List(TestCase("3,0,4,0,99","1",List("1")))
  override def paramsA = List("1")

  override def testSetB = List(TestCase("3,9,8,9,10,9,4,9,99,-1,8","1",List("8")), TestCase("3,9,8,9,10,9,4,9,99,-1,8","0",List("7")),
    TestCase("3,9,7,9,10,9,4,9,99,-1,8","1",List("6")), TestCase("3,9,7,9,10,9,4,9,99,-1,8","0",List("9") ),
    TestCase("3,3,1108,-1,8,3,4,3,99","1",List("8")), TestCase("3,3,1108,-1,8,3,4,3,99","0",List("7") ),
    TestCase("3,3,1107,-1,8,3,4,3,99","1",List("7")), TestCase("3,3,1107,-1,8,3,4,3,99","0",List("8") ),
    TestCase("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9","0",List("0")), TestCase("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9","1",List("8")) ,
    TestCase("3,3,1105,-1,9,1101,0,0,12,4,12,99,1","0",List("0")), TestCase("3,3,1105,-1,9,1101,0,0,12,4,12,99,1","1",List("8"))
  )
  override def paramsB = List("5")

  override def solutionA(input: List[String], params: List[String]) = {
    val program = input(0).split(",").map(s => s.toLong)
    OpcodeProcessor.processDay5OppCode(Program(program,0),params.map(p => p.toLong)).output.reverse.head
    //solution: 11193703
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val program = input(0).split(",").map(s => s.toLong)
    OpcodeProcessor.processDay5OppCode(Program(program,0),params.map(p => p.toLong)).output.head
    //solution 12410607
  }
}
