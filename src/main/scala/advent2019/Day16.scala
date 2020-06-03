package advent2019

import java.util.Calendar

import scala.collection.mutable.ListBuffer

object Day16 extends Day(16){
  override def testSetA = List(TestCase("12345678","23845678"),TestCase("80871224585914546619083218645595","24176176"),
    TestCase("19617804207202209144916044189917","73745418"),TestCase("69317163492948606335995924319873","52432133"))

  override def testSetB = List(TestCase("03036732577212944063491565474664","84462026"),
    TestCase("02935109699940807407585447034323","78725270"),TestCase("03081770884921959731165446850517","53553731"))

  override def solutionA(input: List[String], params: List[String]) = {
    val Loops = 100
    var sequence = input(0).split("").map(s =>s.toInt)

    sequence = calcFFT(sequence,Loops)
    sequence.foldLeft("")(_ + _).substring(0,8)
  }

  override def solutionB(input: List[String], params: List[String]) = {

    val Loops = 100
    var sequence = input(0).split("").map(s =>s.toInt)
    sequence = Array.fill(10000)(sequence).flatten
    val offset = input(0).substring(0,7).toInt

    sequence = calcFFT(sequence,Loops)
    sequence.foldLeft("")(_ + _).substring(offset,offset+8)
  }

  def calcFFT(inputSequence: Array[Int],loops: Int): Array[Int] = {
    val start = Calendar.getInstance().getTimeInMillis
    var sequence = inputSequence
    0 until loops foreach { a =>
      val newSequence = Array.ofDim[Int](sequence.size)
      for (n <- 1 to sequence.size) {
        if (n % 100000 == 0) println(s"${n} of ${sequence.size} " + (Calendar.getInstance().getTimeInMillis - start) + "ms")
        val size = sequence.size
        for (i <- 0 until size / (4 * n)+1) {
          for (t <- n - 1 + i * 4 * n until Math.min(2 * n - 1 + i * 4 * n, size)) newSequence(n - 1) += sequence(t)
          for (t <- 3 * n - 1 + i * 4 * n until Math.min(3 * n - 1 + i * 4 * n + n, size)) newSequence(n - 1) -= sequence(t)
        }
      }
      sequence = newSequence.map(f => Math.abs(f) % 10)
      println(a)
    }
    sequence
  }

  def slightlyFastersolutionA(input: List[String], params: List[String]): String = {
    val Iterations = 100
    var sequence = input(0).split("").map(s =>s.toInt).toList

    for(loop <- 0 until Iterations){
      var newSequence = ListBuffer[Int]()
      for(n <- 1 to sequence.size) {
        var newDigit = 0
        for(i <- 0 until sequence.size/n){
          newDigit += sequence.slice(n-1+i*4*n, Math.min(2*n-1+i*4*n, sequence.size)).sum
          newDigit -= sequence.slice(3*n-1+i*4*n, Math.min(3*n-1+i*4*n+n, sequence.size)).sum
        }
        newSequence +=Math.abs(newDigit)%10
      }
      sequence = newSequence.toList
      // println(s"${loop}: - ${sequence}")
    }

    sequence.foldLeft("")(_ + _).substring(0,8)
  }


  def oldSolutionA(input: List[String], params: List[String]) = {
    val BasePhase = List(0,1,0,-1)
    val Iterations = 100
    var sequence = input(0).split("").map(s =>s.toInt).toList
    var patterns = List[List[Int]]()
    for(i <- 1 to sequence.size) patterns = patterns :+ calcPattern(BasePhase,i)

    for(loop <- 0 until Iterations){
      var newSequence = ListBuffer[Int]()
      for(pattern <- patterns){
        var calcNext = 0
        for(digit <- 0 until sequence.size) calcNext += sequence(digit)*pattern((digit+1)%pattern.size)
        newSequence += Math.abs(calcNext%10)
      }
      sequence = newSequence.toList
      println(s"${loop}: - ${sequence}")
    }
    sequence.foldLeft("")(_ + _).substring(0,8)
  }

  def calcPattern(inputSeq: List[Int],seqNr: Int): List[Int] = {
    val result = ListBuffer[Int]()
    for(n <- inputSeq) result ++= List.fill(seqNr)(n)
    result.toList
  }
}
