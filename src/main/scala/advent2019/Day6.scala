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
      if(orbitMap.getOrElse(start,List()).contains(end)) return List(end)
      if(orbitMap.getOrElse(end,List()).contains(start)) return List(end)
      else{
        //searchDown
        var newPath = List[String]()
        for(downString <- orbitMap.getOrElse(start,List()))
          {
            if(!path.contains(downString)) {
              val downPath = findPath(downString, end, orbitMap, path:+ downString)
              if (downPath.contains(end) &&
                (newPath.size == 0 || downPath.size < newPath.size)) newPath = downPath :+ downString
            }
          }
        //searchUp
        for(upString <- orbitMap.keySet.filter(p => orbitMap.getOrElse(p,List()).contains(start)))
          {
            if(!path.contains(upString)){
              val upPath = findPath(upString,end,orbitMap,path:+ upString)
              if (upPath.contains(end) &&
                (newPath.size == 0 || upPath.size < newPath.size)) newPath = upPath :+ upString
            }
          }
        newPath
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
