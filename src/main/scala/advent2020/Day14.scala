package advent2020

object Day14 extends Day{
  override def day() = 14

  override def testSetA = List(TestCase("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0","165"))

  override def testSetB = List(TestCase("mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1","208"))

  override def solutionA(input: List[String], params: List[String]) = {
    val decoder = new Decoder()
    input.foreach(s => decoder.processInput1(s))
    decoder.getMemory.values.sum.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val decoder = new Decoder()
    input.foreach(s => decoder.processInput2(s))
    decoder.getMemory.values.sum.toString
  }

  class Decoder{
    val parser = "mem\\[([0-9]+)\\] = ([0-9]+)".r
    var mask: String = ""
    var memory = Map[Long,Long]()

    def processInput1(input: String): Unit = processInput(input,updateMemory1)
    def processInput2(input: String): Unit = processInput(input,updateMemory2)

    private def processInput(input:String,memoryUpdator: (Long,Long)=>Unit) = {
      if(input.startsWith("mask")) updateMask(input.substring(7))
      else{
        val memCommand = parser.findAllMatchIn(input).next()
        memoryUpdator(memCommand.group(1).toInt,memCommand.group(2).toInt)
      }
    }

    private def updateMask(input:String): Unit = mask = input

    private def updateMemory1(address:Long,value:Long): Unit = {
      var bitString = value.toBinaryString.reverse.padTo(mask.size,'0').reverse
      for(i <- 0 until mask.size) if(mask.charAt(i) != 'X') bitString = bitString.updated(i,mask.charAt(i))
      memory = memory + (address -> java.lang.Long.parseLong(bitString,2) )
    }

    private def updateMemory2(address:Long,value:Long): Unit = writeMemory2(address.toLong,value,mask)

    private def writeMemory2(address:Long,value:Long,bitMask: String): Unit ={
      def updateBit(bits: String,loc:Int,value:Char) = java.lang.Long.parseLong(bits.updated(loc,value),2)

      if(bitMask == "".padTo(mask.size,'0')) memory = memory + (address -> value)
      else{
        val index = bitMask.indexWhere(_ != '0')
        val memoryBits = address.toBinaryString.reverse.padTo(mask.size,'0').reverse
        val newBitMask = bitMask.updated(index,'0')
        bitMask(index) match {
          case '1' => writeMemory2(updateBit(memoryBits,index,'1'),value,newBitMask)
          case 'X' => writeMemory2(updateBit(memoryBits,index,'0'),value,newBitMask)
                      writeMemory2(updateBit(memoryBits,index,'1'),value,newBitMask)
          case _ => throw new Exception("Unexpected value found: "+bitMask(index))
        }
      }
    }

    def getMemory = memory
  }
}
