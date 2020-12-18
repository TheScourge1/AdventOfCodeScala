package advent2020

object Day17 extends Day {
  override def day() = 17

  override def testSetA = List(TestCase(".#.\n..#\n###","112"))

  override def testSetB = List(TestCase(".#.\n..#\n###","848"))

  override def solutionA(input: List[String], params: List[String]) = {
    var space = new Space(input.size,input(0).size,1,0)
    for(i <- 0 until input.size;j <- 0 until input(0).size) space.update(input(i).charAt(j) == '#',(i,j,0))
    for(i<- 0 until 6) space = space.evolve()
    space.countActiveSpaces.toString
  }

  override def solutionB(input: List[String], params: List[String]) = {
    var space = new Space4(input.size,input(0).size,1,1,0)
    for(i <- 0 until input.size;j <- 0 until input(0).size) space.update(input(i).charAt(j) == '#',(i,j,0,0))
    for(i<- 0 until 6) space = space.evolve()
    space.countActiveSpaces.toString
  }

  class Space(xSize:Int,ySize:Int,zSize:Int,offset:Int) {
    val space = Array.ofDim[Boolean](xSize,ySize, zSize)

    def update(value: Boolean, coord: (Int, Int, Int)) =
      space(coord._1)(coord._2)(coord._3) = value
    def get(coord: (Int, Int, Int)) = space(coord._1)(coord._2)(coord._3)
    def printSpace = {
      for (k <- 0 until space(0)(0).size) {
        for (i <- 0 until space(0).size) {
          for(j <- 0 until space.size) print(if (space(i)(j)(k)) '#' else '.')
          println
        }
        println
      }
    }

    def evolve():Space = {
      val newSpace = new Space(xSize+2,ySize+2,zSize+2,offset+1)
      for (k <- -1 to space(0)(0).size;
           j <- -1 to space(0).size;
           i <- -1 to space.size) {
        var neighbourCount = 0
        for (kn <- Math.max(0, k - 1) to Math.min(space(0)(0).size-1, k + 1);
             jn <- Math.max(0, j - 1) to Math.min(space(0).size-1, j + 1);
             in <- Math.max(0, i - 1) to Math.min(space.size-1, i + 1))
          if (!(i == in && j == jn && k == kn) && get((in, jn, kn))) neighbourCount += 1

        if (i>=0 && j>=0 && k>=0 && i<space.size && j<space(0).size && k < space(0)(0).size && get((i, j, k)))
          if (neighbourCount != 2 && neighbourCount != 3) newSpace.update(false, (i+1, j+1, k+1))
          else newSpace.update(true, (i+1, j+1, k+1))
        else if (neighbourCount == 3) newSpace.update(true, (i+1, j+1, k+1))
          else newSpace.update(false, (i+1, j+1, k+1))
      }
      newSpace
    }

    def countActiveSpaces : Int = {
      (for (k <- 0 until space(0)(0).size; j <- 0 until space(0).size; i <- 0 until space.size) yield get(i,j,k))
        .count(_ == true)
    }
  }

  class Space4(xSize:Int,ySize:Int,zSize:Int,wSize:Int,offset:Int) {
    val space = Array.ofDim[Boolean](xSize, ySize, zSize, wSize)

    def update(value: Boolean, coord: (Int, Int, Int, Int)) =
      space(coord._1)(coord._2)(coord._3)(coord._4) = value

    def get(coord: (Int, Int, Int, Int)) = space(coord._1)(coord._2)(coord._3)(coord._4)

    def printSpace = {
      for (w <- 0 until space(0)(0).size) {
        for (k <- 0 until space(0)(0).size) {
          for (i <- 0 until space(0).size) {
            for (j <- 0 until space.size) print(if (space(i)(j)(k)(w)) '#' else '.')
            println
          }
          println(s"z=${k}, w=${w}\n")
        }
      }
    }

      def evolve(): Space4 = {
        val newSpace = new Space4(xSize + 2, ySize + 2, zSize + 2, wSize + 2, offset + 1)
        for (w <- -1 to space(0)(0)(0).size;
             k <- -1 to space(0)(0).size;
             j <- -1 to space(0).size;
             i <- -1 to space.size) {
          var neighbourCount = 0
          for (wn <- Math.max(0, w - 1) to Math.min(space(0)(0)(0).size - 1, w + 1);
               kn <- Math.max(0, k - 1) to Math.min(space(0)(0).size - 1, k + 1);
               jn <- Math.max(0, j - 1) to Math.min(space(0).size - 1, j + 1);
               in <- Math.max(0, i - 1) to Math.min(space.size - 1, i + 1))
            if (!(i == in && j == jn && k == kn && w == wn) && get((in, jn, kn, wn))) neighbourCount += 1

          if (i >= 0 && j >= 0 && k >= 0 && w >= 0 && i < space.size && j < space(0).size
            && k < space(0)(0).size && w < space(0)(0)(0).size && get((i, j, k, w)))
            if (neighbourCount != 2 && neighbourCount != 3) newSpace.update(false, (i + 1, j + 1, k + 1, w + 1))
            else newSpace.update(true, (i + 1, j + 1, k + 1, w + 1))
          else if (neighbourCount == 3) newSpace.update(true, (i + 1, j + 1, k + 1, w + 1))
          else newSpace.update(false, (i + 1, j + 1, k + 1, w + 1))
        }
        newSpace
      }

      def countActiveSpaces: Int = {
        (for (w <- 0 until space(0)(0)(0).size; k <- 0 until space(0)(0).size;
              j <- 0 until space(0).size; i <- 0 until space.size) yield get(i, j, k, w))
          .count(_ == true)
      }
    }
}
