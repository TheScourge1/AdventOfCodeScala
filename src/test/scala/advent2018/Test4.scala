package advent2018

import org.scalatest.FunSuite

class Test4 extends FunSuite {

  private val ex = Exercise4

  test("Exercise4.ex1") {
    assert(ex.ex1FromFile("2018/ex4_test1.txt") === "240")

    println("Solution 1: "+ex.ex1FromFile("2018/ex4_1.txt"))
  }

  test("Exercise4.ex2") {
    assert(ex.ex2FromFile("2018/ex4_test1.txt") === "?")

    println("Solution 2: "+ex.ex2FromFile("2018/ex4_1.txt"))
  }
}