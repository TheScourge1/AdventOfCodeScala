package advent2018

import collection.mutable

object Exercise1 extends AdventBase{

  override def ex1(input: Seq[String]): String = {
    var result = 0

    for(s <- input)  result += getValue(s)
    return ""+result
  }

  override def ex2(input: Seq[String]): String = {
    var result = 0

    var resultSet = mutable.HashSet.empty[Int]
    while(true) {
      for (s <- input) {
        result += getValue(s)
        if (resultSet.contains(result)) return "" + result
        else resultSet.add(result)
      }
    }

    return "error"
  }

  def getValue(str: String): Int = {
    if(str.startsWith("+")) return str.substring(1).toInt
    else if(str.startsWith("-")) return  -1* str.substring(1).toInt
    else throw new Exception("Unknown input data" + str)
  }
}
