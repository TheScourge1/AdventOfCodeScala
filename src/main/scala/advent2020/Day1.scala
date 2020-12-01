package advent2020

object Day1 extends Day{

  override def day(): Int = 1
  override def testSetA = List(TestCase("1721\n979\n366\n299\n675\n1456","514579"))
  override def testSetB = List(TestCase("1721\n979\n366\n299\n675\n1456","241861950"))

  override def solutionA(input: List[String], params: List[String]):String = {
    val lst = input.map(_.toInt)

    for(i <- 0 until lst.size;j <- 0 until i)
        if(lst(i)+lst(j) == 2020) return (lst(i)*lst(j)).toString

    "NOT FOUND!"
  }

  override def solutionB(input: List[String], params: List[String]):String = {
    val lst = input.map(_.toInt)

    for(i <- 0 until lst.size;j<-0 until i;k<-0 until j)
      if(lst(i)+lst(j)+lst(k)  == 2020) return (lst(i)*lst(j)*lst(k)).toString


    "NOT FOUND!"
  }


}
