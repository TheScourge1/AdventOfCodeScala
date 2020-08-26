package advent2019

object Day22 extends Day(22){
  override def testSetA = List(TestCase("deal with increment 7\ndeal into new stack\ndeal into new stack","6",List("10","8")),
    TestCase("cut 6\ndeal with increment 7\ndeal into new stack","4",List("10","1")),
    TestCase("deal with increment 7\ndeal with increment 9\ncut -2","9",List("10","9")),
    TestCase("deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\n" +
      "deal with increment 9\ndeal with increment 3\ncut -1","3",List("10","8")),
  )

  override def paramsA: List[String] = List("10007","2019")
  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]) = {
    val cardCount = params(0).toInt
    val returnCard = params(1).toInt

    val cutPattern = "cut (-?[0-9]*)".r
    val stackPattern = "deal into new stack".r
    val incrementPattern = "deal with increment ([0-9]+)".r

    var deck = initDeck(cardCount)

    for(action <- input){
      deck = action match {
        case cutPattern(n) => cutCards(deck,n.toInt)
        case stackPattern() => newStack(deck)
        case incrementPattern(n) => incrementCards(deck,n.toInt)
        case _ => throw new Exception("Cannot process input action: \n"+action)
      }
    }

    println(deck)
    deck.indexOf(returnCard).toString
  }

  def initDeck(size: Int): List[Int] = List.tabulate[Int](size)(n => n)

  def newStack(deck: List[Int]): List[Int] = deck.reverse

  def cutCards(deck: List[Int], n:Int): List[Int] = {
    if(n >=0) deck.slice(n,deck.size)++deck.slice(0,n)
    else deck.slice(deck.size+n,deck.size)++deck.slice(0,deck.size+n)
  }

  def incrementCards(deck: List[Int], n: Int): List[Int] = {
    val resArr = Array.ofDim[Int](deck.size)
    var counter = 0
    for(i <- 0 until deck.size) {
      resArr(counter) = deck(i)
      counter = (counter+n)%deck.size
    }
    resArr.toList
  }

  override def solutionB(input: List[String], params: List[String]) = {

    "TODO"
  }
}
