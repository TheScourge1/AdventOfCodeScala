package advent2020

object Day9 extends Day{
  override def day() = 9

  override def testSetA = List()

  override def testSetB = List()

  override def solutionA(input: List[String], params: List[String]):String = {
    val cypher = input.map(s =>s.toLong)
    val preamble = 25
    getInvalidNumber(cypher,preamble).toString
  }

  override def solutionB(input: List[String], params: List[String]):String = {
    val cypher = input.map(s =>s.toLong)
    val preamble = 25
    val invalidNumber = getInvalidNumber(cypher,preamble)

    for(i <- 0 until cypher.size){
      var j = i
      while(cypher.slice(i,j).sum < invalidNumber && j <= cypher.size) j+=1
      if((j-i)>=2 && cypher.slice(i,j).sum == invalidNumber)
        return (cypher.slice(i,j).min + cypher.slice(i,j).max).toString
    }

    "NOT FOUND"
  }

  def getInvalidNumber(cypher: List[Long],preamble:Int): Long ={
    for(i <- preamble until cypher.size){
      var found = false
      for(j<- i-preamble until i;k <- j+1 until i)
        if(cypher(j)+cypher(k) == cypher(i)) found = true
      if(!found) return cypher(i)
    }
    -1L
  }
}
