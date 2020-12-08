package advent2020

import jdk.internal.util.xml.impl.Input

object Day8 extends Day{
  override def day() = 8

  override def testSetA = List(TestCase("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6","5"))
  override def testSetB = List(TestCase("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6","8"))

  override def solutionA(input: List[String], params: List[String]):String = {
    execProg(new Program(input)).getAcc.toString
  }

  override def solutionB(input: List[String], params: List[String]):String = {
    val replaceVal = Map("nop" -> "jmp","jmp" -> "nop")
    for(i<-0 until input.size){
      val currentOpp = input(i).slice(0,3)
      if(replaceVal.keySet.contains(currentOpp)){
        val newRes = execProg(new Program(input.updated(i,replaceVal(currentOpp) + input(i).slice(3,input(i).length))))
        if(newRes.isFinished) return newRes.getAcc.toString
      }
    }
    "NO SOLUTION FOUND!"
  }

  def execProg(prog: Program) : Program = {
    var posHistory = Set[Int]()
    while(!posHistory.contains(prog.currentPos) && !prog.isFinished) {
      posHistory = posHistory + prog.currentPos
      prog.executeInstruction()
    }
    prog
  }

  class Program(inputProg: List[String]){
    private var position = 0
    private var acc = 0L
    private val prog = inputProg.map(s => Instruction(s.split(" ").head,s.split(" ").last.toLong))

    def executeInstruction(): Unit ={
      prog(position).name match {
        case "nop" => position +=1
        case "acc" => acc += prog(position).param
                      position +=1
        case "jmp" => position += prog(position).param.toInt
      }
    }

    def currentPos = position
    def getAcc = acc
    def isFinished = position == prog.size
  }

  case class Instruction(name: String, param:Long)
}
