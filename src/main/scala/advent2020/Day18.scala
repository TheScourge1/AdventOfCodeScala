package advent2020

object Day18 extends Day{
  override def day() = 18

  override def testSetA = List(TestCase("2 * 3 + (4 * 5)","26"),TestCase("5 + (8 * 3 + 9 + 3 * 4 * 3)","437"),TestCase("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))","12240"),TestCase("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2","13632"))
  override def testSetB = List(TestCase("1 + (2 * 3) + (4 * (5 + 6))","51"),TestCase("2 * 3 + (4 * 5)","46"),TestCase("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))","669060"),TestCase("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2","23340"))

  override def solutionA(input: List[String], params: List[String]) = {
    input.map(s => calculateLineA(s.replace("("," ( ").replace(")"," ) ")
      .split(" ").filter(_ != "").toList)).sum.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    input.map(s => calculateLineB(s.replace("("," ( ").replace(")"," ) ")
      .split(" ").filter(_ != "").toList)).sum.toString
  }

  def calculateLineA(commands: List[String]): Long = {
    var stack = List[String]()
    var i = 0
    while(i < commands.size){
      commands(i) match{
        case "(" => val closeInd = i+1+findClosingBracket(commands.drop(i+1))
                    stack = stack :+ calculateLineA(commands.slice(i+1,closeInd)).toString
                    i = closeInd +1
        case _ => stack = stack :+ commands(i)
                  i+=1
      }
    }
    calcStackA(stack)
  }

  def calculateLineB(commands: List[String]): Long = {
    var stack = List[String]()
    var i = 0
    while(i < commands.size){
      commands(i) match{
        case "(" => val closeInd = i+1+findClosingBracket(commands.drop(i+1))
          stack = stack :+ calculateLineB(commands.slice(i+1,closeInd)).toString
          i = closeInd +1
        case _ => stack = stack :+ commands(i)
          i+=1
      }
    }
    calcStackB(stack)
  }


  def findClosingBracket(commands: List[String]): Int = {
    var bracketCounter = 1
    for(i <- 0 until commands.size){
      commands(i) match {
        case "(" => bracketCounter +=1
        case ")" => bracketCounter -= 1
                  if(bracketCounter == 0) return i
        case _ =>
      }
    }

    throw new Exception("No closing bracket found in: "+commands)
  }

  def calcStackA(stack: List[String]): Long ={
    var result = stack(0).toLong
    for(i <- 1 until stack.size by 2) result = stack(i) match {
      case "+" => result + stack(i+1).toLong
      case "*" => result * stack(i+1).toLong
      case _ => throw new Exception("Unexpected value found: " + stack(i))
    }
    result
  }

  def calcStackB(stack: List[String]): Long ={
    if(stack.contains("+")){
      val mulIndex = stack.indexOf("+")
      calcStackB((stack.slice(0,mulIndex-1) :+ (stack(mulIndex-1).toLong+stack(mulIndex+1).toLong).toString)
        ++ stack.slice(mulIndex+2,stack.size))
    }
    else calcStackA(stack)
  }
}
