package advent2018

import org.scalatest.FunSuite

class Test1 extends FunSuite {

  val ex = Exercise1

  test("Exercise1.ex1") {
    assert(ex.ex1("inp") === "outp")

    println("Solution 1: "+ex.ex1FromFile("ex1_1.txt"))
  }

  test("Exercise1.ex2") {
    assert(ex.ex2("inp") === "outp")

    println("Solution 2: "+ex.ex2FromFile("ex1_1.txt"))
  }
}