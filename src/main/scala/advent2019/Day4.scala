package advent2019

object Day4 extends Day(4) {

  override def testSetA = List(TestCase("111111-111111","1"),TestCase("223450-223450","0"),TestCase("123789-123789","0"))
  override def inputA() = {"236491-713787"}

  override def testSetB = List(TestCase("377333-377333","0"),TestCase("112233-112233","1"),TestCase("223450-223450","0"),TestCase("111122-111122","1"),TestCase("123444-123444","0"))
  override def inputB() = {"236491-713787"}

  override def solutionA(input: List[String], params: List[String]) = {
    val low = input(0).split("-")(0).toInt
    val high = input(0).split("-")(1).toInt

    var  resultList = Vector[Int]()
    for(value <- low to high) {
      var temp = value
      var increasing = true
      var doubleFound = false

      for(i <- 0 until 5) {
        if(temp%10 < (temp%100)/10) increasing = false
        if(temp%10 == (temp%100)/10) doubleFound = true
        temp=temp/10
        }
      if(increasing && doubleFound) resultList = resultList :+ value
    }

    resultList.size.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val low = input(0).split("-")(0).toInt
    val high = input(0).split("-")(1).toInt

    var  resultList = Vector[Int]()
    for(value <- low to high) {
      var temp = value
      var increasing = true
      var doubleFound = false

      while(temp >= 10) {
        if(temp%10 < (temp%100)/10) increasing = false
        if((temp%10 == (temp%100)/10))
            if(temp%10 == (temp%1000)/100) while(temp%10 == (temp%1000)/100) temp=temp/10
            else doubleFound = true
        temp=temp/10
      }
      if(increasing && doubleFound) resultList = resultList :+ value
    }
    println(resultList)
    resultList.size.toString
  }
}
