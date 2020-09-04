package advent2019

import java.util.Calendar

object Day22 extends Day(22){
  override def testSetA = List(TestCase("deal with increment 7\ndeal into new stack\ndeal into new stack","6",List("10","8")),
    TestCase("cut 6\ndeal with increment 7\ndeal into new stack","4",List("10","1")),
    TestCase("deal with increment 7\ndeal with increment 9\ncut -2","9",List("10","9")),
    TestCase("deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\n" +
      "deal with increment 9\ndeal with increment 3\ncut -1","3",List("10","8")),
  )

  override def paramsA: List[String] = List("10007","2019")
  override def paramsB: List[String] = List("119315717514047","2020","101741582076661")
  override def testSetB = List(TestCase("deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\n" +
    "deal with increment 9\ndeal with increment 3\ncut -1","8",List("10","3","5")))

  override def solutionA(input: List[String], params: List[String]) = {
    val deckSize = params(0).toInt
    var location = params(1).toLong

    val cutPattern = "cut (-?[0-9]*)".r
    val stackPattern = "deal into new stack".r
    val incrementPattern = "deal with increment ([0-9]+)".r

    for(action <- input){
      location = action match {
        case cutPattern(n) => locationCut(location,n.toLong,deckSize)
        case stackPattern() => locationStack(location,0,deckSize)
        case incrementPattern(n) => locationIncrement(location,n.toLong,deckSize)
        case _ => throw new Exception("Cannot process input action: \n"+action)
      }
    }
    location.toString
  }

  def locationStack(location: Long,noopParam: Long, deckSize: Long):Long = deckSize - location - 1
  def locationCut(location: Long,cutSize:Long, deckSize: Long): Long = (deckSize + location - cutSize)%deckSize
  def locationIncrement(location: Long,increment:Long, deckSize: Long): Long = (location*increment)%deckSize

  def sourceLocationCut(location: Long,cutSize:Long, deckSize: Long): Long = (deckSize + location + cutSize)%deckSize
  def sourceLocationStack(location: Long,noopParam: Long, deckSize: Long): Long = deckSize - location - 1

  def sourceLocationIncrement(location: Long,increment:Long, deckSize: Long): Long = {
    val egdc = extendedGDC(increment,deckSize)
    val result = mulMod(location,egdc.x,deckSize)%deckSize
    if(result < 0) result +deckSize
    else result
  }

  override def solutionB(input: List[String], params: List[String]):String = {
    val cardCount = params(0).toLong
    val requestedPosition = params(1).toLong
    val ittToExecute = params(2).toLong

    var cache = Map[Long,Int]()
    val reverseActionList = getReverseActionList(input)
  //  val actionList = getActionList(input)
    var position = requestedPosition

    for(i <- 0 until 10000000) {
  /*    for(j <- 0 until actionList.size) {
        val prevPos = position
        position = reverseActionList(j)._2(position,reverseActionList(j)._1,cardCount)
        if(prevPos != actionList(actionList.size-1-j)._2(position,actionList(actionList.size-1-j)._1,cardCount))
          throw new Exception(s"invalid reverse at ${j} ${input(actionList.size-1-j)}: ${prevPos} - ${position}")
      }*/

      for (action <- reverseActionList) position = action._2(position,action._1,cardCount)

      if(cache.keySet.contains(position)) {
        println(s"\ncacheHit found after: ${i} itts: "+position)
        val ittStartPos = cache.get(position).get
        val loopSize = i - ittStartPos
        val positionInLoop = (ittToExecute - ittStartPos)%loopSize
        return cache.toList.filter(_._2 == ittStartPos+positionInLoop).head._1.toString
      }
      else cache = cache + (position -> i)
      println(position)
      if(i % 50000 == 0) print(".") // Lower than 37006955654608
                                    //            34436278498096
    }

    println("")
    position.toString
  }

  def getReverseActionList(actions: List[String]): List[(Long,(Long,Long,Long) => Long)] = {
      val cutPattern = "cut (-?[0-9]*)".r
      val stackPattern = "deal into new stack".r
      val incrementPattern = "deal with increment ([0-9]+)".r
      var reverseActionList = List[(Long,(Long,Long,Long) => Long)]()

      for (action <- actions.reverse) {
        val actionFunction =  action match {
          case cutPattern(n) => (n.toLong,sourceLocationCut(_,_,_))
          case stackPattern() => (0L,sourceLocationStack(_,_,_))
          case incrementPattern(n) => (n.toLong,sourceLocationIncrement(_,_,_))
          case _ => throw new Exception("Cannot process input action: \n" + action)
        }
        reverseActionList = reverseActionList :+ actionFunction
      }
      reverseActionList
  }

  def getActionList(actions: List[String]): List[(Long,(Long,Long,Long) => Long)] = {
    val cutPattern = "cut (-?[0-9]*)".r
    val stackPattern = "deal into new stack".r
    val incrementPattern = "deal with increment ([0-9]+)".r
    var actionList = List[(Long,(Long,Long,Long) => Long)]()

    for (action <- actions) {
      val actionFunction =  action match {
        case cutPattern(n) => (n.toLong,locationCut(_,_,_))
        case stackPattern() => (0L,locationStack(_,_,_))
        case incrementPattern(n) => (n.toLong,locationIncrement(_,_,_))
        case _ => throw new Exception("Cannot process input action: \n" + action)
      }
      actionList= actionList :+ actionFunction
    }
    actionList
  }

  def extendedGDC(a: Long, b:Long): Egdc = {
    if(a == 0) Egdc(b,0,1)
    else {
      val egdc = extendedGDC(b%a,a)
      val x = egdc.y - b/a * egdc.x
      val y = egdc.x
      Egdc(egdc.b,x,y)
    }
  }

  def mulMod(a:Long,b:Long,m:Long):Long ={
    var a1 = a%m
    var b1 = b%m
    var res = 0L
    if(a1<0) a1 +=m
    if(b1<0) b1 +=m

    while(b1 > 0) {
      if (b1 % 2 == 1) res = (res + a1) % m
      a1 = a1 * 2 % m
      b1 /= 2
    }
    res
  }

  case class Egdc(b:Long,x:Long,y:Long)

}
