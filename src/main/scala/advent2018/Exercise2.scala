package advent2018

import scala.collection.mutable

object Exercise2 extends AdventBase{

  override def ex1(input: Seq[String]): String = {
    val twos = input.count( p => containsMultiple(p,2))
    val threes = input.count( p => containsMultiple(p,3))
    ""+twos * threes
  }

  override def ex2(input: Seq[String]): String = {
    for(s: String <- input){
      val pairSeq = input.filter(s2 => differsOneDigit(s,s2))
      if(pairSeq.size == 1) return getMatchingString(s,pairSeq.head)
    }
    return "error"
  }

  def containsMultiple(s : String,cnt: Int) : Boolean = {
    var letterMap = mutable.HashMap.empty[Char,Int]
    s.foreach( c => letterMap.update(c,letterMap.getOrElseUpdate(c,0)+1))
    if(letterMap.values.filter(p => p == cnt).size > 0) return true
    false
  }

  def differsOneDigit(s1: String, s2: String): Boolean = {
    if(getMatchingString(s1,s2).size == s1.size-1) return true
    false
  }

  def getMatchingString(s1: String, s2: String): String = {
    var buff = new mutable.StringBuilder()
    for(i <- 0 until s1.size )
      if(s1.substring(i,i+1) == s2.substring(i,i+1)) buff.append(s1.substring(i,i+1))

    buff.toString()
  }
}
