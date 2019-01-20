package advent2018

import scala.util.matching.Regex


object Exercise3 extends AdventBase{

  override def ex1(input: Seq[String]): String = {
    var rectangles = input.map( s => Rectangle(s))
    val maxX = rectangles.map(r => r.x + r.w).max
    val maxY = rectangles.map(r => r.y + r.h).max

    var visitGrid = createVisitGrid(rectangles)
    ""+ visitGrid.reduceLeft((a1,a2) => a1 ++ a2).count(p => p > 1)
  }

  override def ex2(input: Seq[String]): String = {
    var rectangles = input.map(s => Rectangle(s))
    var visitGrid = createVisitGrid(rectangles)

    return ""+rectangles.find(r => isOverlapped(r,visitGrid)).get.id
  }

  case class Rectangle(var init: String = "#0 @ 0,0: 0x0"){
    val expr : Regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

    val (id,x,y,w,h) = init match {
      case expr(a,b,c,d,e) => (a.toInt,b.toInt,c.toInt,d.toInt,e.toInt)
      case _ => throw new Exception("invalid string: "+init)
    }
  }

  def createVisitGrid(rectangles: Seq[Rectangle]): Array[Array[Int]] = {
    val maxX = rectangles.map(r => r.x + r.w).max
    val maxY = rectangles.map(r => r.y + r.h).max
    var visitGrid = Array.ofDim[Int](maxX,maxY)

    for{r <- rectangles
        i <- r.x until r.x + r.w
        j <- r.y until r.y + r.h} visitGrid(i)(j)+=1

    return visitGrid
  }

  def isOverlapped(r: Rectangle,visitGrid: Array[Array[Int]]): Boolean = {
    for{i <- r.x until r.x + r.w
        j <- r.y until r.y + r.h}
        if(visitGrid(i)(j) > 1) return false
    true
  }

}
