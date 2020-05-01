package advent2019

object OpcodeProcessor {

  case class Program(arr: Array[Int],position: Integer,output:List[String] = List()) {

    def inputRequired(): Boolean = {
      val code = arr(position)%100
      code == 3
    }

    def isFinished(): Boolean = {
      val code = arr(position)%100
      code == 99
    }
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

    var position = prog.position
    var program = prog.arr
    var inputList = input
    var resultList = List[String]()

    while(position != -1){
      val result = inputRequired(program,position) match  {
        case false => executeStep(program,position,-1)
        case true => {
          if(inputList.size == 0) return Program(program,position,resultList)
          val res = executeStep(program,position,inputList.head)
          inputList = inputList.tail
          res
        }
      }
      program = result._1
      position = result._2
      if(result._3.isDefined) resultList = resultList :+ result._3.get
    }

   Program(program,position,resultList)
  }

  def executeStep(arr: Array[Int],position: Integer,input: Int): (Array[Int],Int,Option[String]) = {
    val code = arr(position)%100
    val mode: Array[Int] = Array((arr(position)/100)%10,(arr(position)/1000)%10,(arr(position)/10000)%10)
    code match {
      case 1 => { //add
        arr(arr(position+3)) = getValue(arr,position+1,mode(0))+getValue(arr,position+2,mode(1))
        (arr,position+4,None)
      }
      case 2 => { // mul
        arr(arr(position+3)) = getValue(arr,position+1,mode(0))*getValue(arr,position+2,mode(1))
        (arr,position+4,None)
      }
      case 3 => { // input
        arr(arr(position+1)) = input
        (arr,position+2,None)
      }
      case 4 => { // output
        (arr,position+2,Some(getValue(arr,position+1,mode(0)).toString))
      }
      case 5 => { // != 0 jump
        if(getValue(arr,position+1,mode(0)) != 0)
          (arr,getValue(arr,position+2,mode(1)),None)
        else
          (arr,position+3,None)
      }
      case 6 => { //== 0 jump
        if(getValue(arr,position+1,mode(0)) == 0)
          (arr,getValue(arr,position+2,mode(1)),None)
        else
          (arr,position+3,None)
      }
      case 7 => { //_1 < _2 then 1 else 0
        if (getValue(arr, position + 1, mode(0)) < getValue(arr, position + 2, mode(1))) arr(arr(position + 3)) = 1
        else arr(arr(position + 3)) = 0
        (arr, position + 4, None)
      }
      case 8 => { //_1 = _2 then 1 else 0
        if(getValue(arr,position+1,mode(0)) == getValue(arr,position+2,mode(1))) arr(arr(position+3)) = 1
        else arr(arr(position+3)) = 0
        (arr,position+4,None)
      }
      case 99 => (arr,-1,None)
      case _ => throw new Exception(s"Unknown value ${arr(position)} at ${position}")
    }
  }

  def inputRequired(arr: Array[Int],position: Int): Boolean ={
    val code = arr(position)%100
    code == 3
  }

  def isFinished(arr: Array[Int],position: Int): Boolean ={
    val code = arr(position)%100
    code == 99
  }

  private def getValue(arr: Array[Int],position: Int,mode: Int): Int = {
    mode match {
      case 0 => arr(arr(position))
      case 1 => arr(position)
      case _ => throw new Exception("Unknown parameter code")
    }
  }

}
