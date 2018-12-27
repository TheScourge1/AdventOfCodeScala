package advent2017

object Exercise3 extends AdventBase{

  val LEFT = new Coord(-1,0)
  val RIGHT = new Coord(1,0)
  val UP = new Coord(0,1)
  val DOWN = new Coord(0,-1)
  var directions = Seq(RIGHT,UP,LEFT,DOWN)


  override def ex1(input: Seq[String]): String = {
    val target = input(0).toInt;

    var maxCoord = 0
    var location = new Coord(0,0);

    while(true){
      maxCoord +=1
      for(vector <- directions){
        while(location.isValidMove(maxCoord,vector))
        {
          if(target == location.value+1) return location.distance.toString
          location.move(vector)
        }
      }
    }
    return "Not found"
  }

  override def ex2(input: Seq[String]): String = {
    val target = input(0).toInt;
    val ROWMAX = 1000
    var grid = Array.ofDim[Int](ROWMAX,ROWMAX)

    var maxCoord = 0
    var location = new Coord(0,0);
    grid(ROWMAX/2)(ROWMAX/2) = 1

    while(true){
      maxCoord +=1
      for(vector <- directions){
        while(location.isValidMove(maxCoord,vector))
        {
          var sum = 0
          location.move(vector)
          for(i <- -1 to 1)
            for(j <- -1 to 1) sum+= grid(ROWMAX/2+location.x+i)(ROWMAX/2+location.y+j)
          grid(ROWMAX/2+location.x)(ROWMAX/2+location.y) = sum
          if(target < sum) return sum.toString()
        }
      }
    }

    return "Not found"
  }

  class Coord(var x:Int,var y:Int){
    var value = 0;
    def move(vector: Coord) = {x+=vector.x; y+=vector.y;value+=1}
    def isValidMove(maxCoord: Int,vector: Coord): Boolean = {
      Math.max(Math.abs(x+vector.x),Math.abs(y+vector.y)) <= maxCoord}
    def distance: Int = {Math.abs(x)+Math.abs(y)}
  }

}
