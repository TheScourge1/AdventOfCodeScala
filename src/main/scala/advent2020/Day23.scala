package advent2020

object Day23 extends Day{
  override def day() = 23

  override def testSetA = List(TestCase("389125467","67384529"))

  override def testSetB = List(TestCase("389125467","149245887792"))

  override def solutionA(input: List[String], params: List[String]) = {
    val inputList = input(0).toCharArray.map(c =>c.toString).toList
    val itterations = 100

    var currentCup = initCupRing(inputList)
    for(i <- 0 until itterations){
      val threeCups = removethreeCups(currentCup)._1
      var destinationCup = currentCup.oriPrevCup
      while(cupRingContains(destinationCup,threeCups)) destinationCup = destinationCup.oriPrevCup
      insertCups(threeCups,destinationCup)
      currentCup = currentCup.nextCup
    }

    val oneCup = findCup("1",currentCup)
    printCupRing(oneCup).drop(3).replaceAll("->","")
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val startList = input(0).toCharArray.map(c =>c.toString).toList
    val inputList = startList ++ List.tabulate(1000000)(_+1).drop(startList.size).map(_.toString)
    val itterations = 10000000

    var currentCup = initCupRing(inputList)
    for(i <- 0 until itterations){
      val threeCups = removethreeCups(currentCup)._1
      var destinationCup = currentCup.oriPrevCup
      while(cupRingContains(destinationCup,threeCups)) destinationCup = destinationCup.oriPrevCup
      insertCups(threeCups,destinationCup)
      currentCup = currentCup.nextCup
    }

    val oneCup = findCup("1",currentCup)
    (oneCup.nextCup.getLabel.toLong * oneCup.nextCup.nextCup.getLabel.toLong).toString
  }


  def removethreeCups(currentCup:Cup): (Cup,Cup) = {
    val threeCups = currentCup.nextCup
    currentCup.nextCup = currentCup.nextCup.nextCup.nextCup.nextCup
    currentCup.nextCup.prevCup = currentCup
    threeCups.prevCup = null
    threeCups.nextCup.nextCup.nextCup = null
    (threeCups,currentCup)
  }

  def insertCups(threeCups:Cup,insertLocation:Cup) = {
    threeCups.nextCup.nextCup.nextCup = insertLocation.nextCup
    insertLocation.nextCup.prevCup = threeCups.nextCup.nextCup
    insertLocation.nextCup = threeCups
    threeCups.prevCup = insertLocation
  }

  def cupRingContains(cup:Cup,cupRing:Cup): Boolean ={
    var currentCup = cupRing
    do{
      if(currentCup.getLabel == cup.getLabel) return true
      currentCup = currentCup.nextCup
    } while(currentCup != null && currentCup != cupRing)

    false
  }

  class Cup(label:String){
    var nextCup:Cup = null
    var prevCup:Cup  = null
    var oriNextCup:Cup  = null
    var oriPrevCup:Cup  = null

    def getLabel = label

    override def toString = label
    }


  private def initCupRing(input:List[String]):Cup = {
    val startCup = new Cup(input.head)
    var currentCup = startCup
    var i = 0
    for (s <-input.tail) {
      i+=1
      val newCup = new Cup(s)
      newCup.prevCup = currentCup
      currentCup.nextCup = newCup
      currentCup = newCup
    }
    currentCup.nextCup = startCup
    startCup.prevCup = currentCup
    initOriVals(startCup)
  }

    private def initOriVals(startCup: Cup): Cup = {
      var currentCup = startCup
      var cupsToInit = List[Cup]()
      do{
        if(currentCup.getLabel.toInt+1 == currentCup.nextCup.getLabel.toInt) {
          currentCup.oriNextCup = currentCup.nextCup
          currentCup.nextCup.oriPrevCup = currentCup
        }
        else cupsToInit = cupsToInit :+ currentCup
        currentCup = currentCup.nextCup
      }while(currentCup != startCup)

      val maxCup = maxCupLabel(startCup).toInt
      for(cup <- cupsToInit){
        val label = if(cup.getLabel.toInt >= maxCup) 1 else cup.getLabel.toInt +1
        val nextCup = findCup(label.toString,startCup)
        nextCup.oriPrevCup = cup
        cup.oriNextCup = nextCup
      }

      startCup
    }

    private def findCup(label:String, startCup: Cup): Cup ={
      var currentCup = startCup
      while(currentCup.getLabel != label) currentCup = currentCup.nextCup
      currentCup
    }

    private def maxCupLabel(startCup: Cup):String = {
      var currentCup = startCup
      var maxLabel = startCup.getLabel.toInt
      do {
        if(currentCup.getLabel.toInt > maxLabel) maxLabel = currentCup.getLabel.toInt
        currentCup = currentCup.nextCup
      }while(currentCup != startCup)

      maxLabel.toString
    }

  def printCupRing(startCup: Cup): String={
    var currentCup = startCup
    val result = new StringBuilder()
    do{
      result.append(currentCup.getLabel+"->")
      currentCup = currentCup.nextCup
    }while(currentCup != null && currentCup != startCup)
    result.toString.dropRight(2)
  }
}
