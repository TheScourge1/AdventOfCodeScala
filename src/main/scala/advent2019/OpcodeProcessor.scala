package advent2019

object OpcodeProcessor {

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

  def processDay5OppCode(arr: Array[Int],position: Integer,input: Int,output: List[String]): (Array[Int],List[String]) = {

    val code = arr(position)%100
    val mode: Array[Int] = Array((arr(position)/100)%10,(arr(position)/1000)%10,(arr(position)/10000)%10)

    code match {
      case 1 => {
        arr(arr(position+3)) = getValue(arr,position+1,mode(0))+getValue(arr,position+2,mode(1))
        processDay5OppCode(arr,position+4,input,output)
      }
      case 2 => {
        arr(arr(position+3)) = getValue(arr,position+1,mode(0))*getValue(arr,position+2,mode(1))
        processDay5OppCode(arr,position+4,input,output)
      }
      case 3 => {
        arr(arr(position+1)) = input
        processDay5OppCode(arr,position+2,input,output)
      }
      case 4 => {
        processDay5OppCode(arr,position+2,input,output:+getValue(arr,position+1,mode(0)).toString)
      }
      case 5 => {
        if(getValue(arr,position+1,mode(0)) != 0)
          processDay5OppCode(arr,getValue(arr,position+2,mode(1)),input,output)
        else
          processDay5OppCode(arr,position+3,input,output)
      }
      case 6 => {
        if(getValue(arr,position+1,mode(0)) == 0)
          processDay5OppCode(arr,getValue(arr,position+2,mode(1)),input,output)
        else
          processDay5OppCode(arr,position+3,input,output)
      }
      case 7 => {
        if (getValue(arr, position + 1, mode(0)) < getValue(arr, position + 2, mode(1))) arr(arr(position + 3)) = 1
        else arr(arr(position + 3)) = 0
        processDay5OppCode(arr, position + 4, input, output)
      }
      case 8 => {
        if(getValue(arr,position+1,mode(0)) == getValue(arr,position+2,mode(1))) arr(arr(position+3)) = 1
        else arr(arr(position+3)) = 0
        processDay5OppCode(arr,position+4,input,output)
      }
      case 99 => (arr,output)
      case _ => throw new Exception(s"Unknown value ${arr(position)} at ${position}")
    }
  }

  def getValue(arr: Array[Int],position: Int,mode: Int): Int = {
    mode match {
      case 0 => arr(arr(position))
      case 1 => arr(position)
      case _ => throw new Exception("Unknown parameter code")
    }
  }

}
