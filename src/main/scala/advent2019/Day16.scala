package advent2019

import java.util.Calendar

object Day16 extends Day(16){
  override def testSetA = List(TestCase("12345678","23845678"),TestCase("80871224585914546619083218645595","24176176"),
    TestCase("19617804207202209144916044189917","73745418"),TestCase("69317163492948606335995924319873","52432133"))

  override def testSetB = List(TestCase("03036732577212944063491565474664","84462026"),
    TestCase("02935109699940807407585447034323","78725270"),TestCase("03081770884921959731165446850517","53553731"))

  override def solutionA(input: List[String], params: List[String]) = {
    val Loops = 100
    var sequence = input(0).split("").map(s =>s.toInt)

    sequence = calcFFT(sequence,Loops,1)
    sequence.foldLeft("")(_ + _).substring(0,8)
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val Loops = 100
    var sequence = input(0).split("").map(s =>s.toInt)

    sequence = Array.fill(10000)(sequence).flatten
    val offset = input(0).substring(0,7).toInt

    sequence = calcFFT(sequence,Loops,offset)
    sequence.slice(offset,offset+8).foldLeft("")(_ + _)
  }

  def calcFFT(inputSequence: Array[Int],loops: Int,calcFrom:Int): Array[Int] = {
    var sequence = inputSequence
    0 until loops foreach { a =>
      val newSequence = if(calcFrom > sequence.size/2)
        fastFFT(sequence,calcFrom) else regularFFT(sequence,calcFrom)
      sequence = newSequence.map(f => Math.abs(f) % 10)
    }
    sequence
  }

  def fastFFT(sequence: Array[Int],calcFrom:Int): Array[Int] ={
    val start = Calendar.getInstance().getTimeInMillis
    val newSequence = Array.ofDim[Int](sequence.size)
    newSequence(sequence.size-1) = sequence(sequence.size-1)
    for(n <- sequence.size-2 to calcFrom by -1) newSequence(n) = sequence(n) + newSequence(n+1)
    newSequence
  }

  def regularFFT(sequence: Array[Int],calcFrom:Int): Array[Int] ={
    val size = sequence.size
    val start = Calendar.getInstance().getTimeInMillis
    val newSequence = Array.ofDim[Int](sequence.size)
    for (n <- calcFrom to sequence.size) {
      if (n < size / 2) {
        for (i <- 0 until size / (4 * n) + 1) {
          for (t <- n - 1 + i * 4 * n until Math.min(2 * n - 1 + i * 4 * n, size)) newSequence(n - 1) += sequence(t)
          for (t <- 3 * n - 1 + i * 4 * n until Math.min(3 * n - 1 + i * 4 * n + n, size)) newSequence(n - 1) -= sequence(t)
        }
      }
      else for (t <- n - 1 until size) newSequence(n - 1) += sequence(t)
    }
      newSequence
  }
}
