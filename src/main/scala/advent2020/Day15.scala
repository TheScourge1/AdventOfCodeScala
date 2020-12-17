package advent2020

object Day15 extends Day{
  override def day() = 15

  override def testSetA = List(/*TestCase("0,3,6","436"),*/TestCase("1,3,2","1")/*,TestCase("2,1,3","10"),TestCase("3,1,2","1836")*/)

  override def testSetB = List(TestCase("0,3,6","175594"),TestCase("1,3,2","2578"),TestCase("2,1,3","3544142"),TestCase("3,1,2","362"))

  override def solutionA(input: List[String], params: List[String]) = {
    val startNumbers = input(0).split(",").toList.map(s =>s.toLong)
    var timer = 1L
    var history = Map[Long,Long]()
    for(number <-startNumbers.slice(0,startNumbers.size-1)){
      history = history + (number -> timer)
      timer +=1
    }
    var lastSpoken = startNumbers.last
    while(timer <2020){
      val newSpoken = timer - history.getOrElse(lastSpoken,timer)
      history = history + (lastSpoken -> timer)
      timer += 1
      lastSpoken = newSpoken
      println(s"${timer}\t -> "+lastSpoken)
    }
  /*  for(number <- history.values.toList.sorted) {
      print(s"${number}: ")
      print(history.filter(h=> h._1 == number).keySet.toList.sorted.fold("")(_.toString + _+ ", "))
      println
    }*/

    println
    lastSpoken.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }
}
