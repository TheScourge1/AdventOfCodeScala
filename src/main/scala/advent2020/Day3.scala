package advent2020

object Day3 extends Day{
  override def day() = 3

  override def testSetA = List(TestCase("",""))

  override def testSetB = List(TestCase("",""))

  override def solutionA(input: List[String], params: List[String]) = {
    slopeCount(input,3,1).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val slopes = List((1,1),(3,1),(5,1),(7,1),(1,2))
    var res = 1L
    for(slope <- slopes) res *= slopeCount(input,slope._1,slope._2)
    res.toString
  }

  def slopeCount(input: List[String],right:Int,down: Int): Int = {
    var count = 0
    for(i<- 1 until input.size by down)
      if(input(i).charAt((right*i/down)%input(i).size) == '#') count +=1
    count
  }
}
