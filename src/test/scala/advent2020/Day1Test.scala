package advent2020

import org.scalatest.FunSuite

class Day10Test extends FunSuite{
  val input = Day10.inputA()

  test("Day10.countTest"){
    assert(Day10.countOptions(List(0,3,4)) == 1)
    assert(Day10.countOptions(List(2,3,4)) == 2)

    assert(Day10.countOptions(List(0,2,3,4)) == 3)

  /*  assert(Day10.countOptions(List(1,3,4)) == 1)
    assert(Day10.countOptions(List(2,3,4)) == 2)

    assert(Day10.countOptions(List(1,2,3,4)) == 4)*/
    assert(Day10.countOptions(List(0,1,2,3,4)) == 7)

  }
}
