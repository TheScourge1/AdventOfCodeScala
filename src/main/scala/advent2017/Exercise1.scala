package advent2017

object Exercise1 extends AdventBase{

  override def ex1(input: Seq[String]): String = {
    var result = 0
    val s = input(0)
    for(i <- 0 until s.size)
      if(s.charAt(i) == s.charAt((i+1)%(s.size))) result += s.charAt(i)-'0'
    return ""+result
  }

  override def ex2(input: Seq[String]): String = {
    var result = 0
    val s = input(0)
    for(i <- 0 until s.size)
      if(s.charAt(i) == s.charAt((i+s.size/2)%(s.size))) result += s.charAt(i)-'0'
    return ""+result
  }
}
