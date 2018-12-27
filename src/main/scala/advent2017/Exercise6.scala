package advent2017

import scala.collection.mutable

object Exercise6 extends AdventBase{

  var cycleCount = 0
  var memStates = mutable.HashMap.empty[String,Int]

  override def ex1(input: Seq[String]): String = {

    distributeBlocks(input(0))
    memStates.size.toString
  }

  override def ex2(input: Seq[String]): String = {

    val memState= distributeBlocks(input(0))
    (cycleCount-memStates(memState)).toString
  }

  def distributeBlocks(input: String): String = {
    val memory = input.split("\\s+").map(s => s.toInt)
    cycleCount = 0
    memStates = mutable.HashMap.empty[String,Int]

    while(true){
      val memState = getMemState(memory)
      if(memStates.keySet.contains(memState))  return memState
      else memStates+= memState -> cycleCount
      cycleCount+=1

      var index = getMaxIndex(memory)
      var blockCount = memory(index)
      memory(index) = 0
      while(blockCount > 0){
        index = (index + 1) % memory.size
        memory(index) = memory(index)+1
        blockCount -= 1
      }
    }

    "Error"
  }


  def getMaxIndex(mem: Array[Int]): Int = {
    var result = 0;
    var max = 0;
    for(i <- 0 until mem.size)
      if(mem(i) > max){
        result = i
        max = mem(i)
      }
    result
  }

  def getMemState(mem: Array[Int]): String = {
    var result = new StringBuilder
    for(block <- mem)  result ++= " "+block
    result.toString
  }
}
