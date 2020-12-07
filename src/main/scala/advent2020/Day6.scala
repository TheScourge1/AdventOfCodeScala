package advent2020

object Day6 extends Day{
  override def day() = 6

  override def testSetA = List(TestCase("abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb","11"))
  override def testSetB = List(TestCase("",""))

  override def solutionA(input: List[String], params: List[String]) :String =
    splitGroups(input).map(v => countQuestions(v)).sum.toString

  override def solutionB(input: List[String], params: List[String]) :String =
    splitGroups(input).map(v => countQuestions2(v)).sum.toString


  def splitGroups(input: List[String]): List[List[String]] = {
    val splitIndexes = List(0) ++input.zipWithIndex.filter(_._1 == "").map(_._2)++List(input.size)
    (for(i <-0 until splitIndexes.size-1)
      yield input.slice(splitIndexes(i),splitIndexes(i+1)).filter(_ != "")).toList
  }

  def countQuestions(answers: List[String]): Int =
    answers.map(a => a.toCharArray).flatten.toSet.size

  def countQuestions2(answers: List[String]): Int =
    if(answers.size == 0) 0
    else answers.map(a => a.toCharArray.toSet).fold(answers(0).toCharArray.toSet)(_.intersect(_) ).size

}
