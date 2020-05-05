package advent2019

object OpcodeProcessor {

  case class Program(arr: Array[Long],position: Integer,output:List[String] = List(),baseOffset:Int = 0) {

    def getCode: Int = { (arr(position)%100).toInt}
    def curVal: Long = { arr(position)}

    def getValue(mode: Int):Long = {
      val location: Int = mode match {
        case 0 => arr(position).toInt
        case 1 => position
        case 2 => baseOffset + arr(position).toInt
        case _ => throw new Exception("Unknown parameter code")
      }
      if(location >= arr.size) 0
      else arr(location)
    }

    def write(pos: Long,value: Long): Program = { write(pos.toInt,value)}
    def write(pos: Int,value: Long): Program = {
      if(pos >= arr.size){
        val newArr = new Array[Long](pos+1)
        arr.copyToArray(newArr)
        Program(newArr,position,output,baseOffset).write(pos,value)
      }
      else {
        arr(pos)=value
        Program(arr,position,output,baseOffset)
      }
    }

    def inputRequired(): Boolean = { getCode == 3}

    def isFinished: Boolean = {
      if(position == -1) return true
      getCode == 99
    }

    def move(steps: Long): Program = {
      Program(arr,position+steps.toInt,output,baseOffset)
    }

    def jumpTo(newPosition: Long): Program = {
      Program(arr,newPosition.toInt,output,baseOffset)
    }

    def addOutput(s: String): Program = {
      Program(arr,position,output :+ s,baseOffset)
    }

    def setRelativeBase(baseVal: Long) = { Program(arr,position,output,baseOffset+baseVal.toInt)}
  }

  def processDay2OppCode(arr: Array[Int],position: Integer,input: () => Int,output: (Int) => Unit): (Array[Int]) = {
    arr(position) match {
      case 1 => {
        arr(arr(position+3)) = arr(arr(position+1))+arr(arr(position+2))
        processDay2OppCode(arr,position+4,input,output)
      }
      case 2 => {
        arr(arr(position+3)) = arr(arr(position+1))*arr(arr(position+2))
        processDay2OppCode(arr,position+4,input,output)
      }
      case 3 => {
        arr(arr(position+1)) = input()
        processDay2OppCode(arr,position+2,input,output)
      }
      case 4 => {
        output(arr(position+1))
        processDay2OppCode(arr,position+2,input,output)
      }

      case 99 => arr
      case _ => throw new Exception(s"Unknown value ${arr(position)} at ${position}")
    }
  }

  def processDay5OppCode(prog: Program,input: List[Int]): (Program) = {
    var inputList = input
    var program = prog

    while(!program.isFinished){
      program = program.inputRequired() match  {
        case false => executeStep(program,-1)
        case true => {
          if(inputList.size == 0) return program
          val res = executeStep(program,inputList.head)
          inputList = inputList.tail
          res
        }
      }
    }

   program
  }

  def executeStep(prog:Program,input: Int): (Program) = {

    val mode: Array[Int] = Array((prog.curVal.toInt/100)%10,(prog.curVal.toInt/1000)%10,(prog.curVal.toInt/10000)%10)

    prog.getCode match {
      case 1 => { //add
        prog.write(prog.move(3).curVal , prog.move(1).getValue(mode(0))+prog.move(2).getValue(mode(1)))
            .move(4)
      }
      case 2 => { // mul
        prog.write(prog.move(3).curVal , prog.move(1).getValue(mode(0))*prog.move(2).getValue(mode(1)))
            .move(4)
      }
      case 3 => { // input
        prog.write(prog.move(1).curVal , input)
            .move(2)
      }
      case 4 => { // output
        prog.addOutput(prog.move(1).getValue(mode(0)).toString).move(2)
      }
      case 5 => { // != 0 jump
        if(prog.move(1).getValue(mode(0)) != 0)
          prog.jumpTo(prog.move(2).getValue(mode(1)))
        else
          prog.move(3)
      }
      case 6 => { //== 0 jump
        if(prog.move(1).getValue(mode(0)) == 0)
          prog.jumpTo(prog.move(2).getValue(mode(1)))
        else
          prog.move(3)
      }
      case 7 => { //_1 < _2 then 1 else 0
        (if (prog.move(1).getValue(mode(0)) < prog.move(2).getValue(mode(1)))
          prog.write(prog.move(3).curVal , 1)
        else prog.write(prog.move(3).curVal , 0))
        .move(4)
      }
      case 8 => { //_1 = _2 then 1 else 0
        (if(prog.move(1).getValue(mode(0)) == prog.move(2).getValue(mode(1)))
          prog.write(prog.move(3).curVal , 1)
        else prog.write(prog.move(3).curVal , 0))
        .move(4)
      }
      case 9 => { //change offset base
        prog.setRelativeBase(prog.move(1).curVal)
          .move(2)
      }
      case 99 => Program(prog.arr,-1,prog.output,prog.baseOffset)
      case _ => throw new Exception(s"Unknown value ${prog.curVal} at ${prog.position}")
    }
  }
}
