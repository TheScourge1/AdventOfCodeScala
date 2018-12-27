package advent2017

object Exercise3_2 extends AdventBase{


  override def ex1(input: Seq[String]): String = {
    val target = input(0).toInt
    var s = new Snake(0,0)
    for(i <- 1 until target) s.move
    return s.distance().toString()
  }

  override def ex2(input: Seq[String]): String = {
    val target = input(0).toInt
    val GRID_SIZE = 100
    var grid = Array.ofDim[Int](GRID_SIZE,GRID_SIZE)
    var s = new Snake(0,0)

    grid(GRID_SIZE/2)(GRID_SIZE/2) = 1
    while(grid(GRID_SIZE/2+s.x)(GRID_SIZE/2+s.y) < target) {
      s.move
      var sum = 0
      for (i <- -1 to 1)
        for (j <- -1 to 1)
          sum += grid(GRID_SIZE/2+s.x + i)(GRID_SIZE/2+s.y + j)
      grid(GRID_SIZE/2+s.x)(GRID_SIZE/2+s.y) = sum
    }

    return grid(GRID_SIZE/2+s.x)(GRID_SIZE/2+s.y).toString
  }

  class Snake(var x:Int,var y:Int){
    private val right: Snake => Unit = (s: Snake) => { if(s.x < s.depth) {s.x+=1;s.nextMove = right} else {s.x+=1;s.depth+=1;s.nextMove = up}}
    private val left: Snake => Unit  = (s: Snake) => {if(s.x > -1*s.depth) {s.x-=1;s.nextMove = left} else {s.y-=1;s.nextMove = down }}
    private val up: Snake => Unit  = (s: Snake) => {if( s.y < s.depth) {s.y+=1;s.nextMove = up} else {s.x-=1;s.nextMove = left}}
    private val down: Snake => Unit  = (s: Snake) => { if(s.y > -1*s.depth) {s.y-=1;s.nextMove = down} else {s.x+=1;s.nextMove = right}}

    private var depth = 0
    private var nextMove : Snake => Unit = right

    def move = nextMove(this)
    def distance(): Int = {Math.abs(x)+Math.abs(y)}
  }

}
