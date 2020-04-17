package advent2019

object Day2 extends Day(2){

  override def testSetA = List(TestCase("1,9,10,3,2,3,11,0,99,30,40,50","3500"),TestCase("1,0,0,0,99","2"),
    TestCase("2,3,0,3,99","2"),TestCase("2,4,4,5,99,0","2"),TestCase("1,1,1,4,99,5,6,0,99","30"))

  override def testSetB = List(TestCase(inputB,"1202", List("5290681")))

  override def paramsA = List("12","2")
  override def paramsB = List("19690720")


  override def solutionA(input: List[String],params: List[String]): String = {
    val oppCodes = input.head.split(",").map(s => s.toInt)
    if(params.size > 0 ){
      oppCodes(1)=params(0).toInt
      oppCodes(2)=params(1).toInt
    }
    processOppCode(oppCodes,0)(0).toString()
  }

  override def solutionB(input: List[String],params: List[String]):String = {
    for(i: Int <- 0 until 99;j: Int <- 0 until 99) {
      if(solutionA(input,List(i.toString,j.toString)).equals(params(0)))
        return (100*i+j).toString
    }
    "NO Result Found"
  }

  def processOppCode(arr: Array[Int],position: Integer): Array[Int] = {
    arr(position) match {
      case 1 => {
        arr(arr(position+3)) = arr(arr(position+1))+arr(arr(position+2))
        processOppCode(arr,position+4)
      }
      case 2 => {
        arr(arr(position+3)) = arr(arr(position+1))*arr(arr(position+2))
        processOppCode(arr,position+4)
      }
      case 99 => arr
      case _ => throw new Exception(s"Unknown value ${arr(position)} at ${position}")
    }
  }
}
