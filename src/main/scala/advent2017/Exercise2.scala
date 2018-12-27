package advent2017

object Exercise2 extends AdventBase{

  override def ex1(input: Seq[String]): String = {
    var result = 0
    input.foreach(s => result+=getDifference(s))

    return ""+result
  }

  def getDifference(line: String): Int = {
    var numbers = line.split("\\s+").map(Integer.parseInt)
    return numbers.max - numbers.min;
  }

  override def ex2(input: Seq[String]): String = {
    var result = 0
    input.foreach(s => result+=getDivision(s))

    return ""+result
  }

  def getDivision(line: String): Int = {
    var numbers = line.split("\\s+").map(Integer.parseInt)
    numbers = numbers.sorted
    for(i <- 0 until numbers.length)
      for(j <- i+1 until numbers.length)
          if(numbers(j)%numbers(i) == 0) return numbers(j) / numbers(i)

    throw new Exception("No division found")
  }
}
