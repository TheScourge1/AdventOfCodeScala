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
  override def testSetB = List(/*TestCase("deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\n" +
    "deal with increment 9\ndeal with increment 3\ncut -1","8",List("10","3","5"))*/)

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

  def sourceLocationCut(location: Long,cutSize:Long, deckSize: Long): Long = Mcalc(location,deckSize).add(Mcalc(cutSize,deckSize)).v
  def sourceLocationStack(location: Long,noopParam: Long, deckSize: Long): Long = deckSize - location - 1
  def sourceLocationIncrement(location: Long,increment:Long, deckSize: Long): Long = Mcalc(location,deckSize).div(Mcalc(increment,deckSize)).v

  override def solutionB(input: List[String], params: List[String]):String = {
    val cardCount = params(0).toLong
    val endPos = params(1).toLong
    val itts = params(2).toLong

    val fx = getLinearFunction(input,cardCount) //Forward function of type ax+b

    // Xn = X0 * a^n + (a^n -1) / (a-1) * b

    val a = Mcalc(fx._1,cardCount)
    val b = Mcalc(fx._2,cardCount)
    val Xn = Mcalc(endPos,cardCount)
    val one = Mcalc(1,cardCount)
    val n = itts

    val subRes1 = (a.pow(n).sub(one)).mul(b)
    val subRes2 = a.sub(one)
    val subRes3 = Xn.sub((subRes1.div(subRes2)))
    val res = subRes3.div(a.pow(n))

    res.v.toString
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

  def getSourceFromTarget(target:Long,input: List[String],cardCount:Long): Long = {
    val actionList = getReverseActionList(input)
    var res = target
    for (action <- actionList) res = action._2(res,action._1,cardCount)
    res
  }

  /**
   *
   * @return (a,b) represent formula y=ax + b
   */
  def getLinearFunction(input: List[String],cardCount: Long): (Long,Long) = {
    val y1 = 1L
    val y2 = 2L
    val x1 = getSourceFromTarget(y1,input,cardCount)
    val x2 = getSourceFromTarget(y2,input,cardCount)

    val a = (Mcalc(y1,cardCount).sub(Mcalc(y2,cardCount))).div((Mcalc(x1,cardCount).sub(Mcalc(x2,cardCount)))).v
    val b = Mcalc(y2,cardCount).sub(Mcalc(a,cardCount).mul(Mcalc(x2,cardCount))).v
    (a,b)
  }

  // Xn = X0 * a^n + (a^n -1) / (a-1) * b
  def forwardN(a:Mcalc, b:Mcalc, x0: Mcalc,n:Long): Mcalc = {
    val one = Mcalc(1,a.m)
    val part1 = x0.mul(a.pow(n))
    val part2 = (a.pow(n).sub(one)).mul(b).div((a.sub(one)))
    part1.add(part2)
  }



  case class Mcalc(v:Long,m:Long){
    def add(b:Mcalc):Mcalc = Mcalc((m+v%m+b.v%m)%m,m)
    def sub(b:Mcalc):Mcalc = Mcalc((m+v%m-b.v%m)%m,m)
    def mul(b:Mcalc):Mcalc = Mcalc((m+mulMod(v,b.v,m))%m,m)
    def div(b:Mcalc):Mcalc = {
      val inv = extendedGDC(b.v,m).x
      Mcalc((mulMod(v,inv,m)+m)%m,m)
    }
    def pow(e:Long) = Mcalc(powMod(v,e,m),m)

    private def mulMod(a:Long,b:Long,m:Long):Long ={
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

    private def powMod(a:Long,e:Long,m:Long):Long = {
      var rem = e
      var res = 1L
      var base = a%m
      while(rem > 0){
        if(rem%2 != 0) res = mulMod(res, base,m)
        rem = rem/2
        base = mulMod(base,base,m)
      }
    res
    }
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
