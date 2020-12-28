package advent2020

object Day20 extends Day{
  override def day() = 20

  override def testSetA = List(TestCase("Day20test.txt","20899048083289"))

  override def testSetB = List(TestCase("Day20test.txt","273"))

  override def solutionA(input: List[String], params: List[String]) = {
    val tiles = readInput(input)
    val cornerTiles = getCornerTiles(tiles).map(_._1)
    cornerTiles.map(t => t.name.toLong).product.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    val tiles = readInput(input)
    val cornerTiles: List[(Tile,List[String])] = getCornerTiles(tiles)
    val edgeTiles = getEdgeTiles(tiles,1)
    val lineSize = edgeTiles.size/4 +2

    var grid = List[List[Tile]](findTopRow(cornerTiles,edgeTiles,lineSize,tiles))
    while(grid.size < lineSize) grid = grid :+ findRow(cornerTiles ++ edgeTiles,grid.last,(tiles.toSet -- grid.flatten).toList)

    val finalImage = stripBorders(grid)
    var imageTile = Tile("image",finalImage.map(s=>s.toCharArray.toList))
    var monsterCount = 0

    for(i <- 0 until 3){
      if(countMonsters(imageTile.grid.map(c => c.foldLeft("")(_+_))) > monsterCount) monsterCount = countMonsters(imageTile.grid.map(c => c.foldLeft("")(_+_)))
      if(countMonsters(imageTile.hFlip.grid.map(c => c.foldLeft("")(_+_))) > 0) monsterCount = countMonsters(imageTile.hFlip.grid.map(c => c.foldLeft("")(_+_)))
      if(countMonsters(imageTile.vFlip.grid.map(c => c.foldLeft("")(_+_))) > 0) monsterCount = countMonsters(imageTile.vFlip.grid.map(c => c.foldLeft("")(_+_)))
      imageTile = imageTile.rotate
    }

    val hashCount = finalImage.foldLeft(0)((a,b) => a + b.count(_ == '#'))
    (hashCount - monsterCount*15).toString
  }

  def countMonsters(image: List[String]) = (for(i<- 0 until image.size;j <- 0 until image(0).size
                                               if(seaMonsterAt(image,(i,j)))) yield 1).sum

  def seaMonsterAt(grid: List[String],offset: (Int,Int)): Boolean = {
    val seaMonster = List("                  # ","#    ##    ##    ###"," #  #  #  #  #  #   ")
    if(grid.size < offset._1 + seaMonster.size || grid(0).size < seaMonster(0).size+ offset._2 ) return false
    for(i <- 0 until seaMonster.size; j <- 0 until seaMonster(i).size)
      if(seaMonster(i).charAt(j) == '#' && grid(offset._1+i).charAt(offset._2 +j) != '#') return false
    true
  }

  def stripBorders(grid:List[List[Tile]]): List[String] = {
    var result = List[String]()
    for(tileLine<- grid)
      for(i <- 1 until tileLine(0).grid.size-1) {
        var line = ""
        for(tile <- tileLine)
          line = line + (for(j <- 1 until tile.grid.size-1) yield tile.grid(i)(j)).foldLeft("")(_+_)
        result = result :+ line
      }

    result
  }

  def findRow(sidedTiles: List[(Tile,List[String])],prevRow:List[Tile],remainingTiles:List[Tile]): List[Tile] = {
    var result = List[Tile](findOnLeftSide(sidedTiles,prevRow(0).getEdges(2),remainingTiles))
    for(i<- 1 until prevRow.size){
      var nextTile = findTileWithEdges(List(result.last.getEdges(1),prevRow(i).getEdges(2)),(remainingTiles.toSet -- result).toList)
      nextTile = rotateTopLeft(nextTile.get,result.last.getEdges(1),prevRow(i).getEdges(2),4)
      result = result :+ nextTile.get
    }
    result
  }

  def findTopRow(cornerTiles:List[(Tile,List[String])],edgeTiles: List[(Tile,List[String])], rowSize:Int,tiles:List[Tile]): List[Tile] = {
    var result = List(findTopLeft(cornerTiles.head._1,cornerTiles.head._2).get)
    val edgeOptions = cornerTiles ++ edgeTiles
    for(i <- 1 until rowSize){
      val optionalTiles = (for(edge <- edgeOptions)
        yield findTileWithEdges(edge._2:+result.last.getEdges(1),(tiles.toSet--result).toList)).flatten
      if(optionalTiles.size != 1) throw new Exception("Unexpected number of tiles for top row: "+optionalTiles)
      val newTile = optionalTiles.last
      val topEdge = edgeOptions.filter(_._1.name == newTile.name).head._2
      val rotatedTile = rotateTopLeft(newTile,result.last.getEdges(1),topEdge.head,4).getOrElse(
        rotateTopLeft(newTile,result.last.getEdges(1),topEdge.last,4).get
      ) // In case of corner two options possible
      result = result :+ rotatedTile
    }

    result
  }

  def findOnLeftSide(sidedTiles: List[(Tile,List[String])],topEdge: String,tilesLeft:List[Tile]): Tile = {
    val optionalTiles = (for(edge <- sidedTiles)
      yield findTileWithEdges(edge._2 :+ topEdge,tilesLeft)).flatten
    if(optionalTiles.size != 1) throw new Exception("Unexpected number of tiles for top row: "+optionalTiles)
    val newTile = optionalTiles.last
    val leftEdge = sidedTiles.filter(_._1.name == newTile.name).head._2

    rotateTopLeft(newTile,leftEdge.head,topEdge,4).getOrElse(
      rotateTopLeft(newTile,leftEdge.last,topEdge,4).get)
  }

  def findTopLeft(tile:Tile,twoEdges:List[String]):Option[Tile] = {
    if(twoEdges.size != 2) None
    else rotateTopLeft(tile,twoEdges.head,twoEdges.last,4) match {
      case Some(tile) => Some(tile)
      case None => rotateTopLeft(tile,twoEdges.last,twoEdges.head,4)
    }
  }

  def rotateTopLeft(tile:Tile,leftEdge:String,topEdge:String,depth:Int): Option[Tile] = {
    def isValidPosition(tile:Tile,leftEdge:String,topEdge:String) = {
     // println(tile.getEdges+ s" (${topEdge}, ${leftEdge} )")
      (tile.getEdges(0) == topEdge || tile.getEdges(0) == topEdge.reverse) &&
       (tile.getEdges(3) == leftEdge || tile.getEdges(3) == leftEdge.reverse)
    }
    if(depth == 0) return None
    else
      if(isValidPosition(tile,leftEdge,topEdge)) return Some(tile)
      else if(isValidPosition(tile.hFlip,leftEdge,topEdge)) return Some(tile.hFlip)
      else if(isValidPosition(tile.vFlip,leftEdge,topEdge)) return Some(tile.vFlip)
      else if(rotateTopLeft(tile.rotate,leftEdge,topEdge,depth-1) != None)
        return rotateTopLeft(tile.rotate,leftEdge,topEdge,depth-1)
      else if(rotateTopLeft(tile.hFlip.rotate,leftEdge,topEdge,depth-1) != None)
        return rotateTopLeft(tile.hFlip.rotate,leftEdge,topEdge,depth-1)
      else if(rotateTopLeft(tile.vFlip.rotate,leftEdge,topEdge,depth-1) != None)
        return rotateTopLeft(tile.vFlip.rotate,leftEdge,topEdge,depth-1)
    None
  }

  def findTileWithEdges(edges: List[String],tiles: List[Tile]): Option[Tile] = {
    var result = tiles.filter(t => t.getEdges.contains(edges.head) || t.getEdges.contains(edges.head.reverse))
    for(edge <- edges.tail)
      result = result.intersect(tiles.filter(t => t.getEdges.contains(edge) || t.getEdges.contains(edge.reverse)))

    if(result.size > 1 ) throw new Exception("muiltiple tiles found for edges: "+edges + " found # = "+result.size)
    if(result.size == 0) None else Some(result.head)
  }

  def getCornerTiles(tiles:List[Tile]) = getEdgeTiles(tiles,2)
  def getEdgeTiles(tiles: List[Tile],edgeCount: Int): List[(Tile,List[String])] = {
    def getEdges(tile:Tile,tiles:List[Tile]):List[String] =
      (tile.getEdges.toSet -- (tiles.toSet - tile).map(e=> e.getEdges++e.getEdges.map(_.reverse)).flatten).toList
    for(tile <- tiles if(getEdges(tile,tiles).size == edgeCount)) yield (tile,getEdges(tile,tiles))
  }

  def readInput(input: List[String]): List[Tile] = {
    if(input.size % 12 != 11) throw new Exception("unexpected size: "+input.size)
    (for(itt <- 0 until input.size by 12)
      yield Tile(readName(input(itt)),readGrid(input.slice(itt+1,itt+11)))).toList
  }

  def readName(line: String):String =  "([0-9])+".r.findFirstIn(line).get
  def readGrid(grid: List[String]):List[List[Char]] = grid.map(s => s.toCharArray.toList)

  case class Tile(name: String,grid: List[List[Char]]) {
    def getEdges : List[String] =  List[String](
        grid(0).foldLeft("")(_+_),
        grid.foldLeft("")(_+_.last),
        grid(grid.size-1).foldLeft("")(_+_),
        grid.foldLeft("")(_+_.head)
      )

    def rotate: Tile = {
      var resGrid = List[List[Char]]()
      for(j <- 0 until grid.size)
        resGrid = resGrid :+  (for(i<- 0 until grid(0).size) yield grid(grid.size-1-i)(j)).toList
      Tile(name,resGrid)
    }
    def vFlip: Tile = Tile(name,grid.reverse)
    def hFlip: Tile = Tile(name,grid.map(l => l.reverse))

    override def toString = s"name: ${name} \n"+ grid.foldLeft("")(_+ _.foldLeft("")(_+_)+"\n")
    override def hashCode = name.hashCode
    override def equals(obj: Any) = obj match {
      case that: Tile => that.name.equalsIgnoreCase(this.name)
      case _ => false
    }
  }
}
