package advent2019

object Day7 extends Day(7){
  override def testSetA = List(TestCase("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0","43210"),
    TestCase("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0","54321"),
    TestCase("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0","65210"))

  override def testSetB = List()

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
        inputVal = OpcodeProcessor.processDay5OppCode(program.clone(),0,List(amp,inputVal),List())._2(0).toInt

      if(inputVal > maxVal) maxVal = inputVal
    }
    maxVal.toString
  }

  override def solutionB(input: List[String], params: List[String]) =  {
    "MISSING SOLUTION"
  }
}
