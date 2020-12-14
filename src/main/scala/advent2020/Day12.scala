package advent2020

object Day12 extends Day{
  override def day() = 12

  override def testSetA = List(TestCase("F10\nN3\nF7\nR90\nF11","25"))

  override def testSetB = List(TestCase("F10\nN3\nF7\nR90\nF11","286"))

  override def solutionA(input: List[String], params: List[String]) = {
    var ship = Ship('E',0,0)
    for(action <- input)
      ship = action.head match {
        case x if List('N', 'E', 'S', 'W').contains(x) => ship.moveDir(x,action.drop(1).toInt)
        case x if List('L','R').contains(x) => ship.turn(x,action.drop(1).toInt)
        case 'F' => ship.forward(action.drop(1).toInt)
        case _ => throw new Exception(s"Unknown action: ${action}")
      }
    (Math.abs(ship.x)+Math.abs(ship.y)).toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    var ship = Ship('E',0,0,(10,1))
    for(action <- input) {
      ship = action.head match {
        case x if List('N', 'E', 'S', 'W').contains(x) => ship.moveDirR(x,action.drop(1).toInt)
        case x if List('L','R').contains(x) => ship.turnR(x,action.drop(1).toInt)
        case 'F' => ship.forwardR(action.drop(1).toInt)
        case _ => throw new Exception(s"Unknown action: ${action}")
      }
    }

    (Math.abs(ship.x)+Math.abs(ship.y)).toString
  }

  case class Ship(direction: Char,x: Int,y:Int,wayPoint:(Int,Int)=(0,0)){
    def moveDir(moveDir:Char,d:Int) = Ship(direction,x+d*coord(moveDir)._1,y+d*coord(moveDir)._2)
    def forward(d:Int) = Ship(direction,x+d*coord(direction)._1,y+d*coord(direction)._2)
    def turn(dir:Char,degr:Int):Ship = dir match {
      case 'R' => Ship(dirs((dirs.indexOf(direction) + degr/90)%4),x,y)
      case 'L' => turn('R',(360-1*degr)%360)
      case _ => throw new Exception(s"Unexpected turn direction: ${dir}")
    }

    def moveDirR(moveDir:Char,d:Int):Ship =
      Ship(direction,x,y,(coord(moveDir)._1*d+wayPoint._1,coord(moveDir)._2*d+wayPoint._2))
    def turnR(dir:Char,degr:Int):Ship = dir match {
      case 'R' => Ship(direction, x, y,rotate(wayPoint,degr))
      case 'L' => turnR('R', (360 - 1 * degr) % 360)
      case _ => throw new Exception(s"Unexpected turn direction: ${dir}")
    }
    def forwardR(d:Int):Ship = Ship(direction,x+wayPoint._1*d,y+wayPoint._2*d,wayPoint)

    private val coord = Map('N'-> (0,1),'S'->(0,-1),'W'-> (-1,0),'E'-> (1,0))
    private val dirs = List('N','E','S','W')
    private def rotate(xy: (Int,Int),degrees:Int):(Int,Int) =
      if(degrees == 0) xy else rotate((xy._2,-1*xy._1),degrees-90)

    def printShip = s"${direction}: (${x},${y}) -> ${wayPoint}"
  }
}
