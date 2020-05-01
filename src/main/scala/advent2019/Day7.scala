package advent2019

import advent2019.OpcodeProcessor.Program

object Day7 extends Day(7){
  override def testSetA = List(TestCase("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0","43210"),
    TestCase("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0","54321"),
    TestCase("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0","65210"))

  override def testSetB = List(TestCase("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5","139629729"),
                TestCase("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001," +
                  "55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10","18216"))

  override def solutionA(input: List[String], params: List[String]) = {
    val program = input(0).split(",").map(s => s.toInt)
    val baseAmpVals = Set(0,1,2,3,4)
    var maxVal = 0

    for(i1 <- 0 to 4;
        i2 <-baseAmpVals-i1;
        i3 <- baseAmpVals-i1-i2;
        i4 <- baseAmpVals-i1-i2-i3;
        i5 <- baseAmpVals-i1-i2-i3-i4){

      var inputVal = 0
      for(amp <- List(i1,i2,i3,i4,i5))
        inputVal = OpcodeProcessor.processDay5OppCode(Program(program,0),List(amp,inputVal)).output(0).toInt

      if(inputVal > maxVal) maxVal = inputVal
    }
    maxVal.toString
  }

  override def solutionB(input: List[String], params: List[String]) =  {
    val program = input(0).split(",").map(s => s.toInt)
    val baseAmpVals = Set(5,6,7,8,9)
    var maxVal = 0

    for(i1 <- 5 to 9;
        i2 <-baseAmpVals-i1;
        i3 <- baseAmpVals-i1-i2;
        i4 <- baseAmpVals-i1-i2-i3;
        i5 <- baseAmpVals-i1-i2-i3-i4){

      val ampVals = List(i1,i2,i3,i4,i5)

      var output = 0
      var amplifiers = Map[Int,Program]()
      while(!amplifiers.get(4).map(f => f.isFinished()).getOrElse(false)) {
        for (i <- 0 to 4) {
          val result = OpcodeProcessor.processDay5OppCode(
            amplifiers.getOrElse(i, Program(program.clone(), 0)),
            if(amplifiers.contains(i)) List[Int](output) else List[Int](ampVals(i), output))
          amplifiers = amplifiers + (i -> result)
          output = result.output(0).toInt
        }
      }
      if(output > maxVal) maxVal = output
    }
    maxVal.toString
  }
}
