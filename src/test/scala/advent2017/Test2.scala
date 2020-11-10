package advent2017

import org.scalatest.FunSuite

class Test2 extends FunSuite {

  val ex = Exercise2

  test("Exercise2.ex1") {
    assert(ex.ex1FromFile("2017/ex2_1test.txt") === "18")

    println("Solution 1: "+ex.ex1FromFile("2017/ex2_1.txt"))
  }

  test("Exercise2.ex2") {
    assert(ex.ex2FromFile("2017/ex2_2test.txt") === "9")

    println("Solution 2: "+ex.ex2FromFile("2017/ex2_1.txt"))
  }
}