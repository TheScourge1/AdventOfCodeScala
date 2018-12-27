package advent2017

import scala.collection.mutable

object Exercise5 extends AdventBase{


  override def ex1(input: Seq[String]): String = {
    var intList = input.map(s => s.toInt).toArray
    var result = execute(intList,a => a+1)

    result.toString
  }

  override def ex2(input: Seq[String]): String = {
    var intList = input.map(s => s.toInt).toArray
    var result = execute(intList,a => {if(a >=3) a-1 else a+1})

    result.toString
  }

  def execute(intList: Array[Int],calcValue: Int => Int): Int = {
    var result = 0
    var index = 0
    while(index >= 0 && index < intList.length){
      val newIndex = index + intList(index)
      intList(index) = calcValue(intList(index))+index
      index = newIndex
      result+=1
    }

    result
  }
}
