package advent2020

object Day13 extends Day{
  override def day() = 13

  override def testSetA = List(TestCase("939\n7,13,x,x,59,x,31,19","295"))

  override def testSetB = List(TestCase("939\n7,13,x,x,59,x,31,19","1068781"),TestCase("1\n1789,37,47,1889","1202161486"),
    TestCase("939\n67,7,59,61","754018"),TestCase("1\n67,x,7,59,61","779210"),TestCase("939\n67,7,x,59,61","1261476"),TestCase("1\n17,x,13,19","782"))

  override def solutionA(input: List[String], params: List[String]) = {
    val arrivalTime = input(0).toLong
    val busses = input(1).split(",").filter(_ != "x").map(_.toLong).toList
    val arrivalTimes = busses.map(bus => bus-arrivalTime%bus)

    val firstBus = arrivalTimes.indexOf(arrivalTimes.min)
    (busses(firstBus)*arrivalTimes(firstBus)).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val busses = input(1).split(",").toList
    var busIds = busses.filter(_ != "x").map(_.toLong)
    val busTimes = (for(bus <- busses.filter(_ != "x")) yield bus.toLong -> busses.indexOf(bus)).toMap

    //Implementing chinese remainder theorem
    val N = busIds.product
    var result = 0L
    for(i <- 0 until busIds.size){
      val Ni = N/busIds(i)
      val ni = busIds(i)
      val ei = extendedGDC(Ni,ni).x
      result = result + (busTimes(busIds(i))*ei*Ni)%N
    }
    (Math.abs(result) % N).toString
  }

  case class Egdc(b:Long,x:Long,y:Long)
  def extendedGDC(a: Long, b:Long): Egdc = {
    if(a == 0) Egdc(b,0,1)
    else {
      val egdc = extendedGDC(b%a,a)
      val x = egdc.y - b/a * egdc.x
      val y = egdc.x
      Egdc(egdc.b,x,y)
    }
  }
}
