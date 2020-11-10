package advent2017

import org.scalatest.FunSuite

class Test7 extends FunSuite {

  val ex = Exercise7

  test("Exercise7.ex1") {
    assert(ex.ex1FromFile("2017/ex7_1test.txt") === "tknk")

    println("Solution 1: "+ex.ex1FromFile("2017/ex7_1.txt"))
  }

  test("Exercise7.ex2") {
    assert(ex.ex2FromFile("2017/ex7_1test.txt") === "60")

    println("Solution 2: "+ex.ex2FromFile("2017/ex7_1.txt"))
  }

}