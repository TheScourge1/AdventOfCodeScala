package advent2020

object Day20 extends Day{
  override def day() = 20

  override def testSetA = List(TestCase("Day20test.txt","20899048083289"))

  override def testSetB = List(TestCase("Day20test.txt","273"))

  override def solutionA(input: List[String], params: List[String]) = {
    val tiles = readInput(input)
    val edgesList = for(tile <- tiles;edge <- tile.getEdges) yield (edge,tile)
    val edgeTiles = for(tile <- tiles
        if (tile.getEdges.toSet -- (tiles.toSet - tile).map(e=> e.getEdges++e.getEdges.map(_.reverse)).flatten).size == 2) yield tile


    println(edgeTiles.map(e => e.name))

  //  tiles.foreach(t => println(t.toString + t.getEdges))
    edgeTiles.map(t => t.name.toLong).product.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {


    "TODO"
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
        grid(grid.size-1).foldLeft("")(_+_),
        grid.foldLeft("")(_+_.head),
        grid.foldLeft("")(_+_.last)
      )

    override def toString = s"name: ${name} \n"+ grid.foldLeft("")(_+ _.foldLeft("")(_+_)+"\n")
  }
}
