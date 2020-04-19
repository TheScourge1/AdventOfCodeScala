package advent2019

object Day3 extends Day(3) {

  override def testSetA = List(TestCase("R8,U5,L5,D3\nU7,R6,D4,L4","6"),
    TestCase("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83","159"),
    TestCase("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7","135"))
  override def testSetB = List(TestCase("R8,U5,L5,D3\nU7,R6,D4,L4","30"),
    TestCase("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83","610"),
    TestCase("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7","410"))

  override def solutionA(input: List[String], params: List[String]) = {
    val wires = executeSteps(input)
    var distance = 0

    for(coord <- wires(0).keys){
      if(wires(1).contains(coord) && (distance == 0 || distance > Math.abs(coord._1) + Math.abs(coord._2)))
        distance = Math.abs(coord._1) + Math.abs(coord._2)
    }

    distance.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val wires = executeSteps(input)

    var steps = 0
    for(coord <- wires(0).keySet.intersect(wires(1).keySet)){
      if(steps == 0 || steps > wires(0).apply(coord)+wires(1).apply(coord))
        steps = wires(0).apply(coord)+wires(1).apply(coord)
    }
    steps.toString
  }

  def executeSteps(input: List[String]): Vector[Map[(Int,Int),Int]] = {
    var wires = Vector[Map[(Int,Int),Int]]()

    for(i: Int <- 0 until input.size) {
      var x, y,steps: Int = 0
      var wire : Map[(Int,Int),Int] = Map()
      val commandList = input(i).split(",")
      for (ss: String <- commandList) {
        ss.head match {
          case 'R' => for(n<- 0 until ss.substring(1).toInt) { x+=1;steps+=1;(if(!wire.contains((x,y))) wire += ((x,y) -> steps))}
          case 'L' => for(n<- 0 until ss.substring(1).toInt) { x-=1;steps+=1;(if(!wire.contains((x,y))) wire += ((x,y) -> steps))}
          case 'U' => for(n<- 0 until ss.substring(1).toInt) { y+=1;steps+=1;(if(!wire.contains((x,y))) wire += ((x,y) -> steps))}
          case 'D' => for(n<- 0 until ss.substring(1).toInt) { y-=1;steps+=1;(if(!wire.contains((x,y))) wire += ((x,y) -> steps))}
        };
      }
      wires = wires:+wire
    }
    wires
  }
}
