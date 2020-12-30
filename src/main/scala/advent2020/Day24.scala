package advent2020

object Day24 extends Day{
  override def day() = 24

  override def testSetA = List(TestCase("Day24test.txt","10"))

  override def testSetB = List(TestCase("Day24test.txt","2208"))

  override def solutionA(input: List[String], params: List[String]) =
    initTiles(input).size.toString


  override def solutionB(input: List[String], params: List[String]) =  {
    var blackTiles = initTiles(input)
    def isAdjacent(a:(Int,Int),b: (Int,Int)) = (Math.abs(a._1-b._1) == 1 || Math.abs(a._1-b._1) == 2) &&
      (Math.abs(a._2-b._2) == 0 || Math.abs(a._2-b._2) == 1)
    def getAdjacentTiles(tile:(Int,Int)): List[(Int,Int)] =
      List((2,0),(-2,0),(1,1),(1,-1),(-1,1),(-1,-1)).map(l => (l._1 + tile._1,l._2 + tile._2))

    for(i <- 0 until 100){
      var newBlackTiles = blackTiles
      var whiteNextToBlack = List[(Int,Int)]()
      for(blackTile <- blackTiles) {
        val adjacentBlacks = blackTiles.filter(b => isAdjacent(blackTile,b))
        if(adjacentBlacks.size == 0 || adjacentBlacks.size > 2) newBlackTiles = newBlackTiles - blackTile
        whiteNextToBlack = whiteNextToBlack ++ getAdjacentTiles(blackTile)
      }
      whiteNextToBlack = whiteNextToBlack.filter(t => !blackTiles.contains(t))

      val blackNeighBourgCount = whiteNextToBlack.groupBy(identity).mapValues(_.size)
      newBlackTiles = newBlackTiles ++ blackNeighBourgCount.filter(_._2 == 2).keySet
      blackTiles = newBlackTiles
      println(s"turn ${i}: ${blackTiles.size}")
    }

    blackTiles.size.toString
  }

  def initTiles(input:List[String]):  Set[(Int,Int)] = {
    var blackTiles = Set[(Int,Int)]()
    for(s<-input){
      val tileLocation = getTileLocation(s)
      if(blackTiles.contains(tileLocation)) blackTiles = blackTiles - tileLocation
      else blackTiles = blackTiles + tileLocation
    }
    blackTiles
  }

  def getTileLocation(line: String): (Int,Int) = {
    def getDoubleStep(step: String):(Int,Int) = step match{
      case "se" => (1,-1)
      case "sw" => (-1,-1)
      case "nw" => (-1,1)
      case "ne" => (1,1)
    }
    var remainingPath = line
    var locations = List[(Int,Int)]()
    while(remainingPath.size > 0){
      remainingPath = remainingPath match{
        case p if p.startsWith("e") => locations = locations:+ (2,0)
                                        remainingPath.drop(1)
        case p if p.startsWith("w") => locations = locations:+ (-2,0)
                                        remainingPath.drop(1)
        case _ => locations = locations :+ getDoubleStep(remainingPath.slice(0,2))
                    remainingPath.drop(2)
      }
    }
    locations.foldLeft((0,0))(((a,b) => (a._1 + b._1,a._2+b._2)))
  }

}
