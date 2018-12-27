package advent2017

import scala.collection.mutable

object Exercise4 extends AdventBase{


  override def ex1(input: Seq[String]): String = {
    var result = 0;
    for(line <- input){
      val set = mutable.HashSet.empty[String]
      for(token <- line.split("\\s+")) set.add(token)
      if(set.size == line.split("\\s+").size) result +=1
    }

    return result.toString
  }

  override def ex2(input: Seq[String]): String = {
    var result = 0;
    for(line <- input){
      val set = mutable.HashSet.empty[String]
      for(token <- line.split("\\s+")) set.add(token.sorted)
      if(set.size == line.split("\\s+").size) result +=1
    }

    return result.toString
  }
}
