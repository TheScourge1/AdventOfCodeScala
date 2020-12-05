package advent2020

object Day5 extends Day {
  override def day() = 5

  override def testSetA = List(TestCase("BFFFBBFRRR","567"),TestCase("FFFBBBFRRR","119"))

  override def testSetB = List(TestCase("",""))

  override def solutionA(input: List[String], params: List[String]) :String= {
    input.map(p => toNumber(p)).sorted.last.toString
  }

  override def solutionB(input: List[String], params: List[String]):String = {
    val seats = input.map(p => toNumber(p)).sorted
    for(i <- 0 until seats.size-1)
      if(seats(i) == seats(i+1)-2) return (seats(i)+1).toString

    "NOT FOUND"
  }

  def toNumber(input: String):Int = {
    input.toCharArray.map(c => c match{
      case 'F' => 0
      case 'B' => 1
      case 'L' => 0
      case 'R' => 1}).foldLeft(0)(_*2+_)
  }
}
