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

    def getParam(param: Int): Long = {
      this.move(param).getValue(getModes(param-1))
    }

    def getModes: Array[Int] = {
      return Array((curVal.toInt/100)%10,(curVal.toInt/1000)%10,(curVal.toInt/10000)%10)
    }

    def write(pos: Long,value: Long,mode: Int): Program = { write(pos.toInt,value,mode)}
    def write(parPos: Int,value: Long,mode: Int): Program = {
      val pos = if(mode == 2) baseOffset + parPos else parPos
      if(pos >= arr.size){
        val newArr = new Array[Long](pos+1)
        arr.copyToArray(newArr)
        Program(newArr,position,output,baseOffset).write(parPos,value,mode)
      }
      else {
        arr(pos)=value
        Program(arr,position,output,baseOffset)
      }
    }

    def writeAt(param: Int,value: Long): Program = {
      write(move(param).curVal,value, getModes(param-1))
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
    prog.getCode match {
      case 1 => { //add
        prog.writeAt(3 ,prog.getParam(1)+prog.getParam(2))
            .move(4)
      }
      case 2 => { // mul
        prog.writeAt(3 , prog.getParam(1)*prog.getParam(2))
            .move(4)
      }
      case 3 => { // input
        prog.writeAt(1 , input)
            .move(2)
      }
      case 4 => { // output
        prog.addOutput(prog.getParam(1).toString)
          .move(2)
      }
      case 5 => { // != 0 jump
        if(prog.getParam(1)!= 0)
          prog.jumpTo(prog.getParam(2))
        else
          prog.move(3)
      }
      case 6 => { //== 0 jump
        if(prog.getParam(1) == 0)
          prog.jumpTo(prog.getParam(2))
        else
          prog.move(3)
      }
      case 7 => { //_1 < _2 then 1 else 0
        (if (prog.getParam(1) < prog.getParam(2))
          prog.writeAt(3 , 1)
        else prog.writeAt(3 , 0))
        .move(4)
      }
      case 8 => { //_1 = _2 then 1 else 0
        (if(prog.getParam(1) == prog.getParam(2))
          prog.writeAt(3 , 1)
        else prog.writeAt(3, 0))
        .move(4)
      }
      case 9 => { //change relative base
        prog.setRelativeBase(prog.getParam(1))
          .move(2)
      }
      case 99 => Program(prog.arr,-1,prog.output,prog.baseOffset)
      case _ => throw new Exception(s"Unknown value ${prog.curVal} at ${prog.position}")
    }
  }
}
