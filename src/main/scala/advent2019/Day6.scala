package advent2019

object Day6 extends Day(6){
  override def testSetA = List(TestCase("Day6_testa.txt","42"))

  override def testSetB = List(TestCase("Day6_testb.txt","5"))

  override def solutionA(input: List[String], params: List[String]) = {

    var orbitMap: Map[String,List[String]] = Map[String,List[String]]()

    for(obj: String <- input){
      val ob = obj.split(raw"[)]")
      val lst = orbitMap.getOrElse(ob(0),List()) :+ ob(1)
      orbitMap = orbitMap + (ob(0) -> lst)
    }
    orbitMap.keySet.filter(p => !orbitMap.values.flatten.toList.contains(p))
        .foldLeft(0)((a,b) => a + getOrbitCount(orbitMap,b,1))
        .toString
  }

  override def solutionB(input: List[String], params: List[String]) ={

    var orbitMap: Map[String,List[String]] = Map[String,List[String]]()

    for(obj: String <- input){
      val ob = obj.split(raw"[)]")
      val lst = orbitMap.getOrElse(ob(0),List()) :+ ob(1)
      orbitMap = orbitMap + (ob(0) -> lst)
    }

    def findPath(start:String,end:String,orbitMap: Map[String,List[String]],path: List[String]) : List[String] = {
      if(orbitMap.getOrElse(start,List()).contains(end) || orbitMap.getOrElse(end,List()).contains(start)) List(end)
      else{
        var pathOptions = List[List[String]]()
        val moveSet = orbitMap.getOrElse(start,List()).toSet
          .union(orbitMap.keySet.filter(p => orbitMap.getOrElse(p,List()).contains(start)))
          .filter(s => !path.contains(s))

        for(move <- moveSet) {
          val downPath = findPath(move, end, orbitMap, path:+ move)
          if (downPath.contains(end)) pathOptions = pathOptions :+ (downPath :+ move)
          }
        pathOptions.sortBy(f => f.size).headOption.getOrElse(List())
      }
    }

    val path = findPath("YOU","SAN",orbitMap,List())
    println(path)
    if(path.contains("SAN")) (path.length-1).toString
    else "PATH NOT FOUND"

  }

  private def getOrbitCount(map: Map[String,List[String]],node: String,depth: Int): Int =
  {
    val nodeList = map.getOrElse(node,List())
    depth*nodeList.size + nodeList.foldLeft(0)(_ + getOrbitCount(map,_,depth+1))
  }
}
