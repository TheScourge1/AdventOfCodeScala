package advent2020

object Day15 extends Day{
  override def day() = 15

  override def testSetA = List(TestCase("0,3,6","436")/*,TestCase("1,3,2","1"),TestCase("2,1,3","10"),TestCase("3,1,2","1836")*/)

  override def testSetB = List(/*TestCase("0,3,6","175594"),TestCase("1,3,2","2578"),TestCase("2,1,3","3544142"),TestCase("3,1,2","362")*/)

  override def solutionA(input: List[String], params: List[String]) = {
    calcOutput(input,2020).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    calcOutput(input,30000000).toString
  }

  def calcOutput(input: List[String],itt:Int):Long = {
    val startNumbers = input(0).split(",").toList.map(s => s.toLong)
    var timer = 1L
    var history = Map[Long, Long]()
    for (number <- startNumbers.slice(0, startNumbers.size - 1)) {
      history = history + (number -> timer)
      timer += 1
    }
      var lastSpoken = startNumbers.last
      while(timer < itt ){
        val newSpoken = timer - history.getOrElse(lastSpoken,timer)
        history = history + (lastSpoken -> timer)
        timer += 1
        lastSpoken = newSpoken
      }
      lastSpoken
    }
}
