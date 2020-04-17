package advent2019

object Day1 extends Day(1){

  override def testSetA = List(TestCase("12","2"),TestCase("14","2"),TestCase("1969","654"),TestCase("100756","33583"))
  override def testSetB = List(TestCase("14","2"),TestCase("1969","966"),TestCase("100756","50346"))

  override def solutionA(input: List[String],params: List[String] = List()) = {
    input.foldRight(0)((a,b) => calcMass(a.toInt)+b).toString
  }

  override def solutionB(input: List[String],params: List[String] = List()) = {
    input.foldRight(0)((a,b) => calcFuelMass(0,a.toInt)+b).toString
  }

  def calcMass(mass: Integer): Integer = {
    Math.floor(mass/3.0).toInt-2
  }

  def calcFuelMass(total:Integer,input: Integer) : Integer = {
    val res = calcMass(input)
    if(res <= 0) total
    else calcFuelMass(total+res,res)
  }

}
