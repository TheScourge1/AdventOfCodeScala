package advent2020

object Day2 extends Day{
  override def day() = 2

  override def testSetA = List(TestCase("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc","2"))
  override def testSetB = List(TestCase("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc","1"))

  override def solutionA(input: List[String], params: List[String]) = {
    input.count(isValidCode(_,codeARule)).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    input.count(isValidCode(_,codeBRule)).toString
  }

  def isValidCode(code:String,rule: (Int,Int,Char,String)=>Boolean): Boolean = {
    val parser = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]*)".r
    code match {
      case parser(from,to,digit,password) => rule(from.toInt,to.toInt,digit.charAt(0),password)
      case _ => throw new Exception(s"Invalid input: ${code}")
    }
  }

  def codeARule(from: Int,to:Int,c:Char,pass:String):Boolean = {
    val cnt = pass.count(_ == c)
    cnt >= from.toInt && cnt <= to.toInt
  }

  def codeBRule(from: Int,to:Int,c:Char,pass:String):Boolean = {
    (pass.slice(from-1,from)+pass.slice(to-1,to)).count(_ == c) == 1
  }
}
