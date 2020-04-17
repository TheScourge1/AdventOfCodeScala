package advent2018

import scala.util.matching.Regex


object Exercise4 extends AdventBase{

  override def ex1(input: Seq[String]): String = {

    val timeStamp = ".([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)."
    val startShiftPattern =  (timeStamp + " Guard #([0-9]+) begins shift").r
    val fallsAsleepPattern = (timeStamp + " falls asleep").r
    val wakeupPattern = (timeStamp + " wakes up").r

    val sortedInput = input.sorted
    var currentGuard = "none"
    var guardStateMap = Map.empty[DateEvent,Array[Int]]

    sortedInput.foreach( i => i match{
      case startShiftPattern(year,month,day,hour,min,id) => currentGuard = id
      case fallsAsleepPattern(year,month,day,hour,min) => sleep(new DateEvent(year+"-"+month+"-"+day,currentGuard),guardStateMap,min.toInt)
      case wakeupPattern(year,month,day,hour,min) => wakeup(new DateEvent(year+"-"+month+"-"+day,currentGuard),guardStateMap,min.toInt)
      case none => println("error: "+i)
    })

   // guardStateMap.foreach((dateEvent, arr) => println(dateEvent.toString()+arr))

    ""
  }

  override def ex2(input: Seq[String]): String = {

    ""
  }

  case class DateEvent(date: String,id: String)

  def sleep(dateEvent: DateEvent,guardStateMap: Map[DateEvent,Array[Int]],minute: Int): Unit ={
    val minuteArr = guardStateMap.getOrElse(dateEvent,new Array[Int](60))
    for(i <- minute until minuteArr.size)
      minuteArr(i) = 0
  }

  def wakeup(dateEvent: DateEvent,guardStateMap: Map[DateEvent,Array[Int]],minute: Int): Unit ={
    val minuteArr = guardStateMap.getOrElse(dateEvent,new Array[Int](60))
    for(i <- minute until minuteArr.size)
      minuteArr(i) = 1
  }
}
