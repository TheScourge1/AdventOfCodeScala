package advent2020

object Day25 extends Day{
  override def day() = 25

  override def testSetA = List(TestCase("5764801\n17807724","14897079"))

  override def testSetB = List(TestCase("",""))

  override def solutionA(input: List[String], params: List[String]) = {
    val key1 = input(0).toLong
    val key2 = input(1).toLong

    val loopSize1 = calcLoopSize(key1,7)
    var value = 1L
    for(i <- 0 until loopSize1.toInt) value = calcValue(value,key2)
    value.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }

  def calcLoopSize(key: Long,subject:Long): Long = {
    var loopSize = 0L
    var value = 1L
    while(value != key) {
      value = calcValue(value,subject)
      loopSize+=1
    }
    loopSize
  }

  def calcValue(input: Long,subject:Long) = input * subject % 20201227
}
