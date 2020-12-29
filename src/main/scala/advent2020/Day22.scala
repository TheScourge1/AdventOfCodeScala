package advent2020

object Day22 extends Day{
  override def day() = 22

  override def testSetA = List(TestCase("Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10","306"))

  override def testSetB = List(TestCase("Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10","291"),TestCase("Player 1:\n43\n19\n\nPlayer 2:\n2\n29\n14","105"))

  override def solutionA(input: List[String], params: List[String]) = {
    var deck1 = readPlayer("1",input)
    var deck2 = readPlayer("2",input)

    while(deck1.size * deck2.size > 0){
      if(deck1.head > deck2.head) deck1 = deck1:+ deck1.head :+ deck2.head
      else deck2 = deck2 :+ deck2.head :+ deck1.head
      deck1 = deck1.drop(1)
      deck2 = deck2.drop(1)
    }
    if(deck1.size > 0) calcResult(deck1).toString else calcResult(deck2).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val deck1 = readPlayer("1",input)
    val deck2 = readPlayer("2",input)

    val result = recursiveCombat(deck1,deck2)
    if(result._1.size > 0) calcResult(result._1).toString else calcResult(result._2).toString
  }

  def recursiveCombat(inputDeck1:List[Int],inputDeck2:List[Int]): (List[Int],List[Int]) = {
    def hashGamestate(d1:List[Int],d2:List[Int]):String = d1.foldLeft("")(_+_) + '_' +d2.foldLeft("")(_+_)
    var history = Set[String]()
    var deck1 = inputDeck1
    var deck2 = inputDeck2

    while(deck1.size * deck2.size > 0){
      val gameState = hashGamestate(deck1,deck2)
      if(history.contains(gameState)) return(deck1,List()) else history = history + gameState

      val player1Won =
      if(deck1.head < deck1.size && deck2.head < deck2.size)
        recursiveCombat(deck1.slice(1,deck1.head+1),deck2.slice(1,deck2.head+1))._1.size > 0
      else
        deck1.head > deck2.head

      if(player1Won) deck1 = deck1:+ deck1.head :+ deck2.head
      else deck2 = deck2 :+ deck2.head :+ deck1.head
      deck1 = deck1.drop(1)
      deck2 = deck2.drop(1)
    }
    (deck1,deck2)
  }

  def readPlayer(playerId: String,input: List[String]): List[Int] = {
    val startRow =input.indexWhere(_ == s"Player ${playerId}:")
    input.drop(startRow+1).takeWhile(_ != "").map(_.toInt)
  }

  def calcResult(deck:List[Int]): Long = (for(i <- 0 until deck.size) yield deck(i)*(deck.size-i)).sum
}
