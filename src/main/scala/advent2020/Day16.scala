package advent2020

object Day16 extends Day{
  override def day() = 16

  override def testSetA = List(TestCase("class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12","71"))

  override def testSetB = List()
  override def solutionA(input: List[String], params: List[String]) = {

    val myTicket = readMyTicket(input)
    val otherTickets = readOtherTickets(input)
    val rules = readRules(input)

    val invalidTickets = for(ticket <- (otherTickets :+ myTicket))
     yield ticket.fields.filter(field => !validateRules(field,rules))

    invalidTickets.flatten.sum.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val myTicket = readMyTicket(input)
    val otherTickets = readOtherTickets(input)
    val rules = readRules(input)
    val validTickets = otherTickets.filter(_.fields.foldLeft(true)((a,b) => a && validateRules(b,rules)))

    val fieldPositions = (for(rule <- rules) yield ticketsCompliantWithRule(validTickets,rule)).toMap
    val uniqueFieldPositions = getUniqueFields(fieldPositions)

    val validFields = uniqueFieldPositions.filter(_._1.startsWith("departure")).values
    (for(i<-validFields) yield myTicket.fields(i).toLong).product.toString
  }

  def readRules(input:List[String]):List[TicketRule] = {
    val parser = "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)".r
    (for(line <- input) yield
      parser.findFirstMatchIn(line).map(m => TicketRule(
        m.group(1),
        List((m.group(2).toInt,m.group(3).toInt),(m.group(4).toInt,m.group(5).toInt))
      ))).flatten
  }

  def readMyTicket(input:List[String]):Ticket = {
    val ticketInfo = input.indexOf("your ticket:")+1
    Ticket(input(ticketInfo).split(",").toList.map(_.toInt))
  }

  def readOtherTickets(input:List[String]):List[Ticket] = {
    val ticketInfo = input.indexOf("nearby tickets:")+1
    (for(i <- ticketInfo until input.size) yield
      (Ticket(input(i).split(",").toList.map(_.toInt)))).toList
  }

  def validateRule(field:Int,rule: TicketRule) =
    rule.ranges.foldLeft(false)((a,b) => a || (field >= b._1 && field <= b._2))

  def validateRules(field:Int,rules: List[TicketRule])  =
    rules.foldLeft(false)(_ || validateRule(field,_))

  def ticketsCompliantWithRule(tickets: List[Ticket], rule: TicketRule): (String,List[Int]) = {
    val positions = for(i <- 0 until tickets.head.fields.size
                        if(tickets.foldLeft(true)((a,b) => a && validateRule(b.fields(i),rule)))) yield i
    rule.name -> positions.toList
  }

  def getUniqueFields(fieldPositions: Map[String,List[Int]]): Map[String,Int] = {
    var remainingFields = fieldPositions
    var uniqueFieldPositions = Map[String,Int]()
    while(remainingFields.keySet.size > 0) {
      val nextSolution = remainingFields.filter(
        (v) => (v._2.toSet -- uniqueFieldPositions.values.toList).size == 1).toList(0)
      uniqueFieldPositions = uniqueFieldPositions + (nextSolution._1 -> (nextSolution._2.toSet -- uniqueFieldPositions.values.toList).head)
      remainingFields = remainingFields - nextSolution._1
    }
    uniqueFieldPositions
  }

  case class TicketRule(name:String,ranges: List[(Int,Int)])
  case class Ticket(fields: List[Int])
}
