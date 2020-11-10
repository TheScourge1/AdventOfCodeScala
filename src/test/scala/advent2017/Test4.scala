package advent2017

import org.scalatest.FunSuite

class Test4 extends FunSuite {

  val ex = Exercise4

  test("Exercise4.ex1") {
    assert(ex.ex1FromFile("2017/ex3_1test.txt") === "2")

    println("Solution 1: "+ex.ex1FromFile("2017/ex3_1.txt"))
  }

  test("Exercise4.ex2") {
    assert(ex.ex2FromFile("2017/ex3_2test.txt") === "3")

    println("Solution 2: "+ex.ex2FromFile("2017/ex3_1.txt"))
  }
}