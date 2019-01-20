package advent2018

import org.scalatest.FunSuite

class Test3 extends FunSuite {

  private val ex = Exercise3

  test("Exercise3.ex1") {
    assert(ex.ex1FromFile("2018/ex3_test1.txt") === "4")

    println("Solution 1: "+ex.ex1FromFile("2018/ex3_1.txt"))
  }

  test("Exercise3.ex2") {
    assert(ex.ex2FromFile("2018/ex3_test1.txt") === "3")

    println("Solution 2: "+ex.ex2FromFile("2018/ex3_1.txt"))
  }
}