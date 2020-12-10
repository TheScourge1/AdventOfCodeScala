package advent2020

object Day10 extends Day{
  override def day() = 10

  override def testSetA = List(TestCase("16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4","35"),TestCase("28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3","220"))

  override def testSetB = List(TestCase("16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4","8"),TestCase("28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3","19208"))

  override def solutionA(input: List[String], params: List[String]) = {
    val joltList = createJoltList(input)
    val lst = for(i <- 1 until joltList.size) yield joltList(i) - joltList(i-1)
    (lst.count(_ == 1) * lst.count(_ == 3)).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val joltList = createJoltList(input)
    groupList(joltList).map(j => countOptions(j).toLong).product.toString
  }

  def createJoltList(input: List[String]):List[Int] = {
    val joltList = input.map(s => s.toInt)
    (joltList :+ (joltList.max+3) :+ 0).sorted
  }

  def groupList(joltList: List[Int]): List[List[Int]] = {
    for(i <- 1 until joltList.size)
      if(joltList(i)-joltList(i-1) == 3) return groupList(joltList.drop(i)) :+ joltList.take(i)
    List(joltList)
  }

  def countOptions(joltList: List[Int]): Int ={
    if(joltList.size <3) 1
    else if(joltList(2)-joltList(0) == 3) 1+countOptions(joltList.drop(1))
    else countOptions(joltList.filter(_ != joltList(1))) + countOptions(joltList.drop(1))
  }
}
