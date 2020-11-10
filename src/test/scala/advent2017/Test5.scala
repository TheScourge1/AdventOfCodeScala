package advent2017

import org.scalatest.FunSuite

class Test5 extends FunSuite {

  val ex = Exercise5

  test("Exercise5.ex1") {
    assert(ex.ex1FromFile("2017/ex5_1test.txt") === "5")

    println("Solution 1: "+ex.ex1FromFile("2017/ex5_1.txt"))
  }

  test("Exercise5.ex2") {
    assert(ex.ex2FromFile("2017/ex5_1test.txt") === "10")

    println("Solution 2: "+ex.ex2FromFile("2017/ex5_1.txt"))
  }
}