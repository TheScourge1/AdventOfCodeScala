package advent2019

import advent2019.Day10.ggd

object Day12 extends Day(12) {
  override def testSetA = List(TestCase("<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>","179",List("10")),
    TestCase("<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>","1940",List("100")))

  override def testSetB = List(TestCase("<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>","2772"))

  override def paramsA = List("1000")
  override def solutionA(input: List[String], params: List[String]) = {
    var positions = readPositions(input)
    val cycles = params(0).toInt
    var velocities = List.fill(input.size)(0,0,0)

    for(i <- 0 until cycles){
      val gravs = getGravities(positions)
      velocities = addVals(velocities,gravs)
      positions = addVals(positions,velocities)
    }

    val kinEnergy = positions.map(p => getEnergy(p))
    val potEnergy = velocities.map(v => getEnergy(v))
    var totalEnergy = 0
    for(i <- 0 until kinEnergy.size) totalEnergy +=kinEnergy(i) * potEnergy(i)
    totalEnergy.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    var positions = readPositions(input)
    var velocities = List.fill(input.size)(0,0,0)

    var prevXState = Set[String]()
    var prevYState = Set[String]()
    var prevZState = Set[String]()
    var xFound, yFound, zFound: Int = 0
    while(xFound == 0 || yFound == 0 || zFound ==0){
      val gravs = getGravities(positions)
      velocities = addVals(velocities,gravs)
      positions = addVals(positions,velocities)
      if(prevXState.contains(getState(positions,velocities,1)) && xFound == 0) xFound = prevXState.size
      if(prevYState.contains(getState(positions,velocities,2)) && yFound == 0) yFound = prevYState.size
      if(prevZState.contains(getState(positions,velocities,3)) && zFound == 0) zFound = prevZState.size
      prevXState = prevXState + getState(positions,velocities,1)
      prevYState = prevYState + getState(positions,velocities,2)
      prevZState = prevZState + getState(positions,velocities,3)
    }

    var resList = getFactors(xFound)
    for(r <- resList) if(yFound % r == 0) yFound = yFound/r
    resList = resList ++ getFactors(yFound)
    for(r <- resList) if(zFound % r == 0) zFound = zFound/r
    resList = resList ++ getFactors(zFound)
    resList.foldLeft(1L)(_ * _).toString
  }

  def readPositions(input: List[String]): List[(Int,Int,Int)] = {
    val pattern = "<x=(-?\\d*), y=(-?\\d*), z=(-?\\d*)>".r
    input.map(s => pattern.findAllMatchIn(s)
      .map(m => (m.group(1).toInt,m.group(2).toInt,m.group(3).toInt))).flatten
  }

  def getGravities(positions: List[(Int,Int,Int)]): List[(Int,Int,Int)] = {
    def vel(a: Int, b: Int) = if (a > b) 1 else if (a < b) -1 else 0
    var result = List[(Int,Int,Int)]()

    for(pp <- positions) {
      result = result :+
              positions.map(p => (vel(p._1, pp._1), vel(p._2, pp._2), vel(p._3, pp._3)))
                .foldLeft((0,0,0))((t1,t2) => (t1._1+t2._1,t1._2+t2._2,t1._3+t2._3))
    }
    result
  }

  def addVals(a: List[(Int,Int,Int)], b: List[(Int,Int,Int)]): List[(Int,Int,Int)] = {
    var res = List[(Int,Int,Int)]()
    for(i <- 0 until a.size)
      res = res :+ ((a(i)._1+ b(i)._1,a(i)._2+b(i)._2,a(i)._3+b(i)._3 ))
    res
  }

  def getState(a: List[(Int,Int,Int)], b: List[(Int,Int,Int)],index: Int): String = {
    index match {
      case 1 => a.map(t => t._1).toString() + b.map(t => t._1).toString()
      case 2 => a.map(t => t._2).toString() + b.map(t => t._2).toString()
      case 3 => a.map(t => t._3).toString() + b.map(t => t._3).toString()
    }
  }

  def getEnergy(a: (Int,Int,Int)): Int = Math.abs(a._1) + Math.abs(a._2) +Math.abs(a._3)

  def getFactors(in: Int): List[Int] = {
    var rem = in
    var factor = 2
    var result = List[Int]()
    while(rem/factor > 2){
      if(rem%factor == 0) {
        result = result :+ factor
        rem = rem/factor
      }
      else factor += 1
    }

    (result :+ rem)
  }
}
