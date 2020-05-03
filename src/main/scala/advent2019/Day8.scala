package advent2019

import scala.collection.mutable.ListBuffer

object Day8 extends Day(8){
  override def testSetA = List()

  override def testSetB =  List(TestCase("0222112222120000","0110",List("2","2")))

  override def solutionA(input: List[String], params: List[String]) = {

    val WIDE = 25
    val TALL = 6

    var layers = ListBuffer[String]()
    for(i <- 0 until input.head.size/(WIDE*TALL)) layers = layers :+ input.head.substring(i*(WIDE*TALL),(i+1)*(WIDE*TALL))

    val countZero = layers.map(s => s.filter(p => p == '0').size)
    val lowestZeroLayer = countZero.indexOf(countZero.min)

    (layers(lowestZeroLayer).count( _ == '1') * layers(lowestZeroLayer).count( _ == '2')).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {

    var WIDE = 25
    var TALL = 6

    if(params.size == 2){
      WIDE = params(0).toInt
      TALL = params(1).toInt
    }

    var layers = List[String]()
    for(i <- 0 until input.head.size/(WIDE*TALL)) layers = layers :+ input.head.substring(i*(WIDE*TALL),(i+1)*(WIDE*TALL))

    def findVisibleDigit(layers: List[String],index: Int) : String = {
      if(layers.size == 0) return "2"
      else if(layers.head(index) == '2') return findVisibleDigit(layers.tail,index)
      else return  layers.head(index).toString
    }

    val result = new StringBuilder()

    for(i <- 0 until WIDE*TALL) result.append(findVisibleDigit(layers,i))
    for(i <- 0 until TALL) println(result.toString.substring(i*WIDE,(i+1)*WIDE))
    result.toString
  }
}
