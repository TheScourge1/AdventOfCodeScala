package advent2018

import org.scalatest.FunSuite

class Test2 extends FunSuite {

  private val ex = Exercise2

  test("Exercise2.ex1") {
    assert(ex.ex1FromFile("2018/ex2_test.txt") === "12")

    println("Solution 1: "+ex.ex1FromFile("2018/ex2_1.txt"))
  }

  test("Exercise1.ex2") {
    assert(ex.ex2FromFile("2018/ex2_test2.txt") === "fgij")

    println("Solution 2: "+ex.ex2FromFile("2018/ex2_1.txt"))
  }
}