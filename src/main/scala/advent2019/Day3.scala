package advent2019

object Day3 extends Day(3) {

  override def testSetA = List(TestCase("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83","159"),
    TestCase("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7","135"))
  override def testSetB = List(TestCase("",""))

  override def solutionA(input: List[String], params: List[String]) = {

    var coordinatesList: List[List[(Int,Int)]] = List[List[(Int,Int)]]()

    for(i: Int <- 0 until input.size) {
      var x, y: Int = 0
      val commandList = input(i).split(",")
      var tempList: List[(Int, Int)] = List[(Int, Int)]()
      for (ss: String <- commandList) {
        ss.head match {
          case 'R' => x += ss.substring(1).toInt
          case 'L' => x -= ss.substring(1).toInt
          case 'U' => y += ss.substring(1).toInt
          case 'D' => x -= ss.substring(1).toInt
        };
        tempList = (x, y)::tempList
     }
      coordinatesList = tempList :: coordinatesList
    }

    var distance = 0
    for(coord: (Int,Int) <- coordinatesList(0)){
      if(coordinatesList(1).contains(coord) && coord._1+coord._2 < distance) distance = coord._1+coord._2
    }

    println(coordinatesList(0))
    println(coordinatesList(1))


    distance.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
  "ERROR"
  }
}
